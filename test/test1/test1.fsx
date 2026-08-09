//#############################################################################
// Aqualis数式処理テスト
let projectname = "test1"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

Compile [Fortran;C99;Python] outputdir projectname "aaa" <| fun ctx ->
    ctx.io.fileOutput "result.dat" <| fun wr ->
    ctx.ch.D "x" <| fun p ->
    ctx.ch.D "y" <| fun q ->
    ctx.ch.d <| fun z1 ->
    ctx.ch.d <| fun z2 ->
        let x = D 1.1
        let y = D 1.0
        p <== x
        q <== y
        //printfn "%d" 1
        ctx.print.s "test001"
        //equation: ((6.2*(4.2/(-7.7)*(-2.1)))-y-6.8)
        let s = (((6.2*(4.2/(-7.7)*(-2.1)))-y-6.8)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((6.2*(4.2/(-7.7)*(-2.1)))-q-6.8)
            z2 <== ((6.2*(4.2/(-7.7)*(-2.1)))-y-6.8)
            wr.tt <| (I 1)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 2
        ctx.print.s "test002"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 2)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 3
        ctx.print.s "test003"
        //equation: (-6.2)
        //printfn "%d" 4
        ctx.print.s "test004"
        //equation: 1.5
        //printfn "%d" 5
        ctx.print.s "test005"
        //equation: ((((-y)+5.6)+(y/(-y)-x+x/4.6)+((-x)))+(x/(y+(-y))+(-1.3)+x*y))
        let s = (((((-y)+5.6)+(y/(-y)-x+x/4.6)+((-x)))+(x/(y+(-y))+(-1.3)+x*y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-q)+5.6)+(q/(-q)-p+p/4.6)+((-p)))+(p/(q+(-q))+(-1.3)+p*q))
            z2 <== ((((-y)+5.6)+(y/(-y)-x+x/4.6)+((-x)))+(x/(y+(-y))+(-1.3)+x*y))
            wr.tt <| (I 5)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 6
        ctx.print.s "test006"
        //equation: (5.2/7.4+(((-6.6)*(-x)))*((y)/(-7.3)*((-y)-8.4*x-(-5.5)))-x)
        let s = ((5.2/7.4+(((-6.6)*(-x)))*((y)/(-7.3)*((-y)-8.4*x-(-5.5)))-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (5.2/7.4+(((-6.6)*(-p)))*((q)/(-7.3)*((-q)-8.4*p-(-5.5)))-p)
            z2 <== (5.2/7.4+(((-6.6)*(-x)))*((y)/(-7.3)*((-y)-8.4*x-(-5.5)))-x)
            wr.tt <| (I 6)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 7
        ctx.print.s "test007"
        //equation: ((y)+y+(0.8)*(-8.6)/7.1)
        let s = (((y)+y+(0.8)*(-8.6)/7.1)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q)+q+(0.8)*(-8.6)/7.1)
            z2 <== ((y)+y+(0.8)*(-8.6)/7.1)
            wr.tt <| (I 7)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 8
        ctx.print.s "test008"
        //equation: 5.0
        //printfn "%d" 9
        ctx.print.s "test009"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 9)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 10
        ctx.print.s "test010"
        //equation: (y+((-8.5)/5.3+(4.6*(-y)+x-7.2))-(2.1-(8.1*(-x))*((-3.8)+1.2)+(-y)-(-2.7))*6.8+(y/4.0+6.3-0.0*y)+x*(-1.0))
        let s = ((y+((-8.5)/5.3+(4.6*(-y)+x-7.2))-(2.1-(8.1*(-x))*((-3.8)+1.2)+(-y)-(-2.7))*6.8+(y/4.0+6.3-0.0*y)+x*(-1.0))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q+((-8.5)/5.3+(4.6*(-q)+p-7.2))-(2.1-(8.1*(-p))*((-3.8)+1.2)+(-q)-(-2.7))*6.8+(q/4.0+6.3-0.0*q)+p*(-1.0))
            z2 <== (y+((-8.5)/5.3+(4.6*(-y)+x-7.2))-(2.1-(8.1*(-x))*((-3.8)+1.2)+(-y)-(-2.7))*6.8+(y/4.0+6.3-0.0*y)+x*(-1.0))
            wr.tt <| (I 10)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 11
        ctx.print.s "test011"
        //equation: ((y*(-y)+(-1.1))*5.7)*8.2*(y*(-4.3)*((-3.4))-4.7)*((-y))+((-y))+x-y
        let s = (((y*(-y)+(-1.1))*5.7)*8.2*(y*(-4.3)*((-3.4))-4.7)*((-y))+((-y))+x-y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q*(-q)+(-1.1))*5.7)*8.2*(q*(-4.3)*((-3.4))-4.7)*((-q))+((-q))+p-q
            z2 <== ((y*(-y)+(-1.1))*5.7)*8.2*(y*(-4.3)*((-3.4))-4.7)*((-y))+((-y))+x-y
            wr.tt <| (I 11)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 12
        ctx.print.s "test012"
        //equation: 2.2
        //printfn "%d" 13
        ctx.print.s "test013"
        //equation: (-8.5)
        //printfn "%d" 14
        ctx.print.s "test014"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 14)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 15
        ctx.print.s "test015"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 15)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 16
        ctx.print.s "test016"
        //equation: 1.1
        //printfn "%d" 17
        ctx.print.s "test017"
        //equation: ((-x)/(((-x)*(-y)*(-y)*y)*(3.5)/8.7-(-1.6)))
        let s = (((-x)/(((-x)*(-y)*(-y)*y)*(3.5)/8.7-(-1.6)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)/(((-p)*(-q)*(-q)*q)*(3.5)/8.7-(-1.6)))
            z2 <== ((-x)/(((-x)*(-y)*(-y)*y)*(3.5)/8.7-(-1.6)))
            wr.tt <| (I 17)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 18
        ctx.print.s "test018"
        //equation: (2.7-6.2)
        //printfn "%d" 19
        ctx.print.s "test019"
        //equation: ((7.0)+y*x-x)
        let s = (((7.0)+y*x-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((7.0)+q*p-p)
            z2 <== ((7.0)+y*x-x)
            wr.tt <| (I 19)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 20
        ctx.print.s "test020"
        //equation: (-4.2)
        //printfn "%d" 21
        ctx.print.s "test021"
        //equation: (((-0.4)+6.3*2.2-(-x))-0.7-(1.6/3.5*x)-(4.1*x+0.7/y*y))+((x/x/2.8))
        let s = ((((-0.4)+6.3*2.2-(-x))-0.7-(1.6/3.5*x)-(4.1*x+0.7/y*y))+((x/x/2.8))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-0.4)+6.3*2.2-(-p))-0.7-(1.6/3.5*p)-(4.1*p+0.7/q*q))+((p/p/2.8))
            z2 <== (((-0.4)+6.3*2.2-(-x))-0.7-(1.6/3.5*x)-(4.1*x+0.7/y*y))+((x/x/2.8))
            wr.tt <| (I 21)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 22
        ctx.print.s "test022"
        //equation: ((((-0.6)-(-2.5)+(-5.8)-(-y)))/5.8-((5.2*(-0.4))+4.3*(-x)*(y+y+7.5*1.8-7.2)))
        let s = (((((-0.6)-(-2.5)+(-5.8)-(-y)))/5.8-((5.2*(-0.4))+4.3*(-x)*(y+y+7.5*1.8-7.2)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-0.6)-(-2.5)+(-5.8)-(-q)))/5.8-((5.2*(-0.4))+4.3*(-p)*(q+q+7.5*1.8-7.2)))
            z2 <== ((((-0.6)-(-2.5)+(-5.8)-(-y)))/5.8-((5.2*(-0.4))+4.3*(-x)*(y+y+7.5*1.8-7.2)))
            wr.tt <| (I 22)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 23
        ctx.print.s "test023"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 23)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 24
        ctx.print.s "test024"
        //equation: 6.1
        //printfn "%d" 25
        ctx.print.s "test025"
        //equation: ((-x))
        let s = (((-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p))
            z2 <== ((-x))
            wr.tt <| (I 25)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 26
        ctx.print.s "test026"
        //equation: (x*(((-5.5)+(-4.5)*1.4/(-6.8)+(-y))*6.7*(x-(-x)-y-(-x))/((-y)/(-7.4)+y+(-y))/y)*2.8-y+(((-x)-y+(-0.7)/(-y))*2.7-(4.7+(-y))))
        let s = ((x*(((-5.5)+(-4.5)*1.4/(-6.8)+(-y))*6.7*(x-(-x)-y-(-x))/((-y)/(-7.4)+y+(-y))/y)*2.8-y+(((-x)-y+(-0.7)/(-y))*2.7-(4.7+(-y))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p*(((-5.5)+(-4.5)*1.4/(-6.8)+(-q))*6.7*(p-(-p)-q-(-p))/((-q)/(-7.4)+q+(-q))/q)*2.8-q+(((-p)-q+(-0.7)/(-q))*2.7-(4.7+(-q))))
            z2 <== (x*(((-5.5)+(-4.5)*1.4/(-6.8)+(-y))*6.7*(x-(-x)-y-(-x))/((-y)/(-7.4)+y+(-y))/y)*2.8-y+(((-x)-y+(-0.7)/(-y))*2.7-(4.7+(-y))))
            wr.tt <| (I 26)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 27
        ctx.print.s "test027"
        //equation: (-0.3)
        //printfn "%d" 28
        ctx.print.s "test028"
        //equation: (-1.2)
        //printfn "%d" 29
        ctx.print.s "test029"
        //equation: 8.0
        //printfn "%d" 30
        ctx.print.s "test030"
        //equation: (-1.8)
        //printfn "%d" 31
        ctx.print.s "test031"
        //equation: (5.2*(-x))
        let s = ((5.2*(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (5.2*(-p))
            z2 <== (5.2*(-x))
            wr.tt <| (I 31)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 32
        ctx.print.s "test032"
        //equation: ((x-2.8)-(-y)-y)
        let s = (((x-2.8)-(-y)-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p-2.8)-(-q)-q)
            z2 <== ((x-2.8)-(-y)-y)
            wr.tt <| (I 32)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 33
        ctx.print.s "test033"
        //equation: ((((-x)))*((-y)+((-y)-7.2))*((-2.4)+((-y)))+y+(-x))
        let s = (((((-x)))*((-y)+((-y)-7.2))*((-2.4)+((-y)))+y+(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-p)))*((-q)+((-q)-7.2))*((-2.4)+((-q)))+q+(-p))
            z2 <== ((((-x)))*((-y)+((-y)-7.2))*((-2.4)+((-y)))+y+(-x))
            wr.tt <| (I 33)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 34
        ctx.print.s "test034"
        //equation: 6.4
        //printfn "%d" 35
        ctx.print.s "test035"
        //equation: (x+(x-(-2.8)+y+(-5.8)/(-4.2))+(-3.5)-(-0.7)-(-6.8)*(x/(-y)+(-8.5))*(-7.3)-((-3.8)+(-y))-((6.7)-8.7-(-1.1)-(-7.0)+(-0.3)/1.0))
        let s = ((x+(x-(-2.8)+y+(-5.8)/(-4.2))+(-3.5)-(-0.7)-(-6.8)*(x/(-y)+(-8.5))*(-7.3)-((-3.8)+(-y))-((6.7)-8.7-(-1.1)-(-7.0)+(-0.3)/1.0))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p+(p-(-2.8)+q+(-5.8)/(-4.2))+(-3.5)-(-0.7)-(-6.8)*(p/(-q)+(-8.5))*(-7.3)-((-3.8)+(-q))-((6.7)-8.7-(-1.1)-(-7.0)+(-0.3)/1.0))
            z2 <== (x+(x-(-2.8)+y+(-5.8)/(-4.2))+(-3.5)-(-0.7)-(-6.8)*(x/(-y)+(-8.5))*(-7.3)-((-3.8)+(-y))-((6.7)-8.7-(-1.1)-(-7.0)+(-0.3)/1.0))
            wr.tt <| (I 35)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 36
        ctx.print.s "test036"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 36)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 37
        ctx.print.s "test037"
        //equation: (6.3-(-y)/5.8*7.1/(-y)+8.0-3.3)-((-x)-(-x)-(-8.7))+3.5-(-2.5)-3.0/1.7+(-x)*(-x)
        let s = ((6.3-(-y)/5.8*7.1/(-y)+8.0-3.3)-((-x)-(-x)-(-8.7))+3.5-(-2.5)-3.0/1.7+(-x)*(-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (6.3-(-q)/5.8*7.1/(-q)+8.0-3.3)-((-p)-(-p)-(-8.7))+3.5-(-2.5)-3.0/1.7+(-p)*(-p)
            z2 <== (6.3-(-y)/5.8*7.1/(-y)+8.0-3.3)-((-x)-(-x)-(-8.7))+3.5-(-2.5)-3.0/1.7+(-x)*(-x)
            wr.tt <| (I 37)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 38
        ctx.print.s "test038"
        //equation: (-2.2)
        //printfn "%d" 39
        ctx.print.s "test039"
        //equation: ((-x)+8.7*(x+(-y)-x)+(-y)*(y*(-x)-(-6.6)-x+y)*x*(x)/((-x)*y+(-5.5)/0.1))
        let s = (((-x)+8.7*(x+(-y)-x)+(-y)*(y*(-x)-(-6.6)-x+y)*x*(x)/((-x)*y+(-5.5)/0.1))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)+8.7*(p+(-q)-p)+(-q)*(q*(-p)-(-6.6)-p+q)*p*(p)/((-p)*q+(-5.5)/0.1))
            z2 <== ((-x)+8.7*(x+(-y)-x)+(-y)*(y*(-x)-(-6.6)-x+y)*x*(x)/((-x)*y+(-5.5)/0.1))
            wr.tt <| (I 39)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 40
        ctx.print.s "test040"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 40)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 41
        ctx.print.s "test041"
        //equation: (-2.4)
        //printfn "%d" 42
        ctx.print.s "test042"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 42)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 43
        ctx.print.s "test043"
        //equation: (((y/(-y)*(-7.8)*x)+(-x)/(7.1))/y-(((-x)+(-2.8)-(-y)*(-4.4)*5.6)*(y+(-5.4)-(-7.0)/(-8.7))+3.4+6.1/1.1)*y*((-y))/(0.1*(-2.4)*(-7.2)-(-0.0))/(-y)-0.3)
        let s = ((((y/(-y)*(-7.8)*x)+(-x)/(7.1))/y-(((-x)+(-2.8)-(-y)*(-4.4)*5.6)*(y+(-5.4)-(-7.0)/(-8.7))+3.4+6.1/1.1)*y*((-y))/(0.1*(-2.4)*(-7.2)-(-0.0))/(-y)-0.3)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((q/(-q)*(-7.8)*p)+(-p)/(7.1))/q-(((-p)+(-2.8)-(-q)*(-4.4)*5.6)*(q+(-5.4)-(-7.0)/(-8.7))+3.4+6.1/1.1)*q*((-q))/(0.1*(-2.4)*(-7.2)-(-0.0))/(-q)-0.3)
            z2 <== (((y/(-y)*(-7.8)*x)+(-x)/(7.1))/y-(((-x)+(-2.8)-(-y)*(-4.4)*5.6)*(y+(-5.4)-(-7.0)/(-8.7))+3.4+6.1/1.1)*y*((-y))/(0.1*(-2.4)*(-7.2)-(-0.0))/(-y)-0.3)
            wr.tt <| (I 43)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 44
        ctx.print.s "test044"
        //equation: (y*((-1.8)-(y*1.6*(-y))/(y-y)/(-y)+(-8.6)/y+y)-(x*(-0.1)/4.7)+(-0.0)+((-y)-(-y)+(-3.0))-((-6.6))/((-y)+(6.3+7.2-(-7.7)/(-x)/(-7.5))+(-x)+(-0.2)*8.3))
        let s = ((y*((-1.8)-(y*1.6*(-y))/(y-y)/(-y)+(-8.6)/y+y)-(x*(-0.1)/4.7)+(-0.0)+((-y)-(-y)+(-3.0))-((-6.6))/((-y)+(6.3+7.2-(-7.7)/(-x)/(-7.5))+(-x)+(-0.2)*8.3))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q*((-1.8)-(q*1.6*(-q))/(q-q)/(-q)+(-8.6)/q+q)-(p*(-0.1)/4.7)+(-0.0)+((-q)-(-q)+(-3.0))-((-6.6))/((-q)+(6.3+7.2-(-7.7)/(-p)/(-7.5))+(-p)+(-0.2)*8.3))
            z2 <== (y*((-1.8)-(y*1.6*(-y))/(y-y)/(-y)+(-8.6)/y+y)-(x*(-0.1)/4.7)+(-0.0)+((-y)-(-y)+(-3.0))-((-6.6))/((-y)+(6.3+7.2-(-7.7)/(-x)/(-7.5))+(-x)+(-0.2)*8.3))
            wr.tt <| (I 44)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 45
        ctx.print.s "test045"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 45)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 46
        ctx.print.s "test046"
        //equation: (-2.6)
        //printfn "%d" 47
        ctx.print.s "test047"
        //equation: ((-x)-y+(-5.2)*((-x))/(-x)-x/0.0+(x*4.6)/((-0.8)*(-y)))
        let s = (((-x)-y+(-5.2)*((-x))/(-x)-x/0.0+(x*4.6)/((-0.8)*(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)-q+(-5.2)*((-p))/(-p)-p/0.0+(p*4.6)/((-0.8)*(-q)))
            z2 <== ((-x)-y+(-5.2)*((-x))/(-x)-x/0.0+(x*4.6)/((-0.8)*(-y)))
            wr.tt <| (I 47)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 48
        ctx.print.s "test048"
        //equation: (7.6+(4.7)/(x+y/0.1+(-0.7)+(-x))/(8.3*(-y)*(-x)/y/(-x))+x/x*x+(-x))
        let s = ((7.6+(4.7)/(x+y/0.1+(-0.7)+(-x))/(8.3*(-y)*(-x)/y/(-x))+x/x*x+(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (7.6+(4.7)/(p+q/0.1+(-0.7)+(-p))/(8.3*(-q)*(-p)/q/(-p))+p/p*p+(-p))
            z2 <== (7.6+(4.7)/(x+y/0.1+(-0.7)+(-x))/(8.3*(-y)*(-x)/y/(-x))+x/x*x+(-x))
            wr.tt <| (I 48)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 49
        ctx.print.s "test049"
        //equation: (-4.5)
        //printfn "%d" 50
        ctx.print.s "test050"
        //equation: (7.2/((-y)/(-x)/(-x)*y)/6.3*2.7-(-2.0)-1.1/(2.1-(-y)/(-5.8)*(-x)-x))
        let s = ((7.2/((-y)/(-x)/(-x)*y)/6.3*2.7-(-2.0)-1.1/(2.1-(-y)/(-5.8)*(-x)-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (7.2/((-q)/(-p)/(-p)*q)/6.3*2.7-(-2.0)-1.1/(2.1-(-q)/(-5.8)*(-p)-p))
            z2 <== (7.2/((-y)/(-x)/(-x)*y)/6.3*2.7-(-2.0)-1.1/(2.1-(-y)/(-5.8)*(-x)-x))
            wr.tt <| (I 50)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 51
        ctx.print.s "test051"
        //equation: 3.1
        //printfn "%d" 52
        ctx.print.s "test052"
        //equation: ((-7.7))
        //printfn "%d" 53
        ctx.print.s "test053"
        //equation: ((-x))
        let s = (((-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p))
            z2 <== ((-x))
            wr.tt <| (I 53)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 54
        ctx.print.s "test054"
        //equation: ((-y)+(-x)*0.8/(-7.5))
        let s = (((-y)+(-x)*0.8/(-7.5))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)+(-p)*0.8/(-7.5))
            z2 <== ((-y)+(-x)*0.8/(-7.5))
            wr.tt <| (I 54)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 55
        ctx.print.s "test055"
        //equation: (2.0)
        //printfn "%d" 56
        ctx.print.s "test056"
        //equation: ((((-x)*(-x)+1.4)/x*y/(2.4*(-4.5)))-x-x)
        let s = (((((-x)*(-x)+1.4)/x*y/(2.4*(-4.5)))-x-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-p)*(-p)+1.4)/p*q/(2.4*(-4.5)))-p-p)
            z2 <== ((((-x)*(-x)+1.4)/x*y/(2.4*(-4.5)))-x-x)
            wr.tt <| (I 56)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 57
        ctx.print.s "test057"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 57)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 58
        ctx.print.s "test058"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 58)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 59
        ctx.print.s "test059"
        //equation: (-4.5)
        //printfn "%d" 60
        ctx.print.s "test060"
        //equation: (-7.4)
        //printfn "%d" 61
        ctx.print.s "test061"
        //equation: (((7.6*(-x)*x*(-x))+(y-1.6))*y)
        let s = ((((7.6*(-x)*x*(-x))+(y-1.6))*y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((7.6*(-p)*p*(-p))+(q-1.6))*q)
            z2 <== (((7.6*(-x)*x*(-x))+(y-1.6))*y)
            wr.tt <| (I 61)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 62
        ctx.print.s "test062"
        //equation: (5.7-8.5*y)
        let s = ((5.7-8.5*y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (5.7-8.5*q)
            z2 <== (5.7-8.5*y)
            wr.tt <| (I 62)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 63
        ctx.print.s "test063"
        //equation: (x/x)
        let s = ((x/x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p/p)
            z2 <== (x/x)
            wr.tt <| (I 63)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 64
        ctx.print.s "test064"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 64)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 65
        ctx.print.s "test065"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 65)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 66
        ctx.print.s "test066"
        //equation: (1.6-y)
        let s = ((1.6-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (1.6-q)
            z2 <== (1.6-y)
            wr.tt <| (I 66)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 67
        ctx.print.s "test067"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 67)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 68
        ctx.print.s "test068"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 68)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 69
        ctx.print.s "test069"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 69)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 70
        ctx.print.s "test070"
        //equation: ((-6.7)/(4.2*(-1.1)*y)*(-x)*x-x-((-x)+x/y)-y+(-4.2)*x)
        let s = (((-6.7)/(4.2*(-1.1)*y)*(-x)*x-x-((-x)+x/y)-y+(-4.2)*x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-6.7)/(4.2*(-1.1)*q)*(-p)*p-p-((-p)+p/q)-q+(-4.2)*p)
            z2 <== ((-6.7)/(4.2*(-1.1)*y)*(-x)*x-x-((-x)+x/y)-y+(-4.2)*x)
            wr.tt <| (I 70)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 71
        ctx.print.s "test071"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 71)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 72
        ctx.print.s "test072"
        //equation: ((-5.1)/(-y)*(-x)-(7.5-(-3.1)-(-2.0)+(-y)-(-6.1)))
        let s = (((-5.1)/(-y)*(-x)-(7.5-(-3.1)-(-2.0)+(-y)-(-6.1)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-5.1)/(-q)*(-p)-(7.5-(-3.1)-(-2.0)+(-q)-(-6.1)))
            z2 <== ((-5.1)/(-y)*(-x)-(7.5-(-3.1)-(-2.0)+(-y)-(-6.1)))
            wr.tt <| (I 72)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 73
        ctx.print.s "test073"
        //equation: (-1.3)
        //printfn "%d" 74
        ctx.print.s "test074"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 74)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 75
        ctx.print.s "test075"
        //equation: 5.4
        //printfn "%d" 76
        ctx.print.s "test076"
        //equation: 6.3
        //printfn "%d" 77
        ctx.print.s "test077"
        //equation: 8.1
        //printfn "%d" 78
        ctx.print.s "test078"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 78)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 79
        ctx.print.s "test079"
        //equation: ((x-1.6*7.7)*(x/(-3.5)+(-x))+(y*(-8.4)+0.4-x)/((-y)*y)*(-y)*(-8.1)-(-8.2)+3.0*(-x)*x/(-7.7)/(-1.8)-3.4/(2.0+(-y))-x-y)
        let s = (((x-1.6*7.7)*(x/(-3.5)+(-x))+(y*(-8.4)+0.4-x)/((-y)*y)*(-y)*(-8.1)-(-8.2)+3.0*(-x)*x/(-7.7)/(-1.8)-3.4/(2.0+(-y))-x-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p-1.6*7.7)*(p/(-3.5)+(-p))+(q*(-8.4)+0.4-p)/((-q)*q)*(-q)*(-8.1)-(-8.2)+3.0*(-p)*p/(-7.7)/(-1.8)-3.4/(2.0+(-q))-p-q)
            z2 <== ((x-1.6*7.7)*(x/(-3.5)+(-x))+(y*(-8.4)+0.4-x)/((-y)*y)*(-y)*(-8.1)-(-8.2)+3.0*(-x)*x/(-7.7)/(-1.8)-3.4/(2.0+(-y))-x-y)
            wr.tt <| (I 79)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 80
        ctx.print.s "test080"
        //equation: ((-0.4))
        //printfn "%d" 81
        ctx.print.s "test081"
        //equation: (x/(-3.0))
        let s = ((x/(-3.0))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p/(-3.0))
            z2 <== (x/(-3.0))
            wr.tt <| (I 81)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 82
        ctx.print.s "test082"
        //equation: (y/(3.2/(-5.1)/y)-(7.7*y*(-y))/(5.0-(8.4*(-y)+(-y)*y)+(-1.6)-y+(-x)-4.1-5.2)-((-8.4)*x-(-y)+(-x)-(-4.5))+(-3.4)*(-y)+y-y*((y*y/2.0)))
        let s = ((y/(3.2/(-5.1)/y)-(7.7*y*(-y))/(5.0-(8.4*(-y)+(-y)*y)+(-1.6)-y+(-x)-4.1-5.2)-((-8.4)*x-(-y)+(-x)-(-4.5))+(-3.4)*(-y)+y-y*((y*y/2.0)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q/(3.2/(-5.1)/q)-(7.7*q*(-q))/(5.0-(8.4*(-q)+(-q)*q)+(-1.6)-q+(-p)-4.1-5.2)-((-8.4)*p-(-q)+(-p)-(-4.5))+(-3.4)*(-q)+q-q*((q*q/2.0)))
            z2 <== (y/(3.2/(-5.1)/y)-(7.7*y*(-y))/(5.0-(8.4*(-y)+(-y)*y)+(-1.6)-y+(-x)-4.1-5.2)-((-8.4)*x-(-y)+(-x)-(-4.5))+(-3.4)*(-y)+y-y*((y*y/2.0)))
            wr.tt <| (I 82)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 83
        ctx.print.s "test083"
        //equation: (-7.7)
        //printfn "%d" 84
        ctx.print.s "test084"
        //equation: ((-7.7))
        //printfn "%d" 85
        ctx.print.s "test085"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 85)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 86
        ctx.print.s "test086"
        //equation: (7.5*0.1/((-x)-6.8/(-y))/y-(-y)+(5.4*x-(-y)-(-y)*(-8.7)))
        let s = ((7.5*0.1/((-x)-6.8/(-y))/y-(-y)+(5.4*x-(-y)-(-y)*(-8.7)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (7.5*0.1/((-p)-6.8/(-q))/q-(-q)+(5.4*p-(-q)-(-q)*(-8.7)))
            z2 <== (7.5*0.1/((-x)-6.8/(-y))/y-(-y)+(5.4*x-(-y)-(-y)*(-8.7)))
            wr.tt <| (I 86)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 87
        ctx.print.s "test087"
        //equation: ((-5.6))
        //printfn "%d" 88
        ctx.print.s "test088"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 88)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 89
        ctx.print.s "test089"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 89)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 90
        ctx.print.s "test090"
        //equation: (-4.1)
        //printfn "%d" 91
        ctx.print.s "test091"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 91)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 92
        ctx.print.s "test092"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 92)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 93
        ctx.print.s "test093"
        //equation: (1.3-3.6+y-5.5)
        let s = ((1.3-3.6+y-5.5)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (1.3-3.6+q-5.5)
            z2 <== (1.3-3.6+y-5.5)
            wr.tt <| (I 93)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 94
        ctx.print.s "test094"
        //equation: (-5.7)
        //printfn "%d" 95
        ctx.print.s "test095"
        //equation: (((0.4/1.5)*(x+(-y)*y-2.6/8.7)+4.0-2.2*(-x)))
        let s = ((((0.4/1.5)*(x+(-y)*y-2.6/8.7)+4.0-2.2*(-x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((0.4/1.5)*(p+(-q)*q-2.6/8.7)+4.0-2.2*(-p)))
            z2 <== (((0.4/1.5)*(x+(-y)*y-2.6/8.7)+4.0-2.2*(-x)))
            wr.tt <| (I 95)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 96
        ctx.print.s "test096"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 96)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 97
        ctx.print.s "test097"
        //equation: ((((-0.3)/(-4.0)*(-x)+5.0)-x*(-x)-(-0.7))/((-x)*5.4-8.1/(x)+((-y)-(-y)/6.7/y))/((-y)*1.6*y*(-y)+x+(-y)-((-2.1)+x/2.6-(-y)+(-x))+x/x+x/(-5.7))*((x/(-x)*(-y)-(-y)/(-7.3))*x*1.3))
        let s = (((((-0.3)/(-4.0)*(-x)+5.0)-x*(-x)-(-0.7))/((-x)*5.4-8.1/(x)+((-y)-(-y)/6.7/y))/((-y)*1.6*y*(-y)+x+(-y)-((-2.1)+x/2.6-(-y)+(-x))+x/x+x/(-5.7))*((x/(-x)*(-y)-(-y)/(-7.3))*x*1.3))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-0.3)/(-4.0)*(-p)+5.0)-p*(-p)-(-0.7))/((-p)*5.4-8.1/(p)+((-q)-(-q)/6.7/q))/((-q)*1.6*q*(-q)+p+(-q)-((-2.1)+p/2.6-(-q)+(-p))+p/p+p/(-5.7))*((p/(-p)*(-q)-(-q)/(-7.3))*p*1.3))
            z2 <== ((((-0.3)/(-4.0)*(-x)+5.0)-x*(-x)-(-0.7))/((-x)*5.4-8.1/(x)+((-y)-(-y)/6.7/y))/((-y)*1.6*y*(-y)+x+(-y)-((-2.1)+x/2.6-(-y)+(-x))+x/x+x/(-5.7))*((x/(-x)*(-y)-(-y)/(-7.3))*x*1.3))
            wr.tt <| (I 97)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 98
        ctx.print.s "test098"
        //equation: (((x-(-0.6)+(-0.0)+x/(-0.6))*y+(-0.0)/((-5.4)/x))/((-1.7)*(-0.2)*4.8+(-1.4)-x)+0.3-((-8.3)-(-x))*((-7.8)+(-y))/(1.3)*(0.4-(-x)/(2.5*(-3.7)+4.1)-(7.5+4.0*2.8/(-y)-(-4.3))/(-2.7)))
        let s = ((((x-(-0.6)+(-0.0)+x/(-0.6))*y+(-0.0)/((-5.4)/x))/((-1.7)*(-0.2)*4.8+(-1.4)-x)+0.3-((-8.3)-(-x))*((-7.8)+(-y))/(1.3)*(0.4-(-x)/(2.5*(-3.7)+4.1)-(7.5+4.0*2.8/(-y)-(-4.3))/(-2.7)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((p-(-0.6)+(-0.0)+p/(-0.6))*q+(-0.0)/((-5.4)/p))/((-1.7)*(-0.2)*4.8+(-1.4)-p)+0.3-((-8.3)-(-p))*((-7.8)+(-q))/(1.3)*(0.4-(-p)/(2.5*(-3.7)+4.1)-(7.5+4.0*2.8/(-q)-(-4.3))/(-2.7)))
            z2 <== (((x-(-0.6)+(-0.0)+x/(-0.6))*y+(-0.0)/((-5.4)/x))/((-1.7)*(-0.2)*4.8+(-1.4)-x)+0.3-((-8.3)-(-x))*((-7.8)+(-y))/(1.3)*(0.4-(-x)/(2.5*(-3.7)+4.1)-(7.5+4.0*2.8/(-y)-(-4.3))/(-2.7)))
            wr.tt <| (I 98)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 99
        ctx.print.s "test099"
        //equation: 3.0
        //printfn "%d" 100
        ctx.print.s "test100"
        //equation: 2.7
        //printfn "%d" 101
        ctx.print.s "test101"
        //equation: 8.4
        //printfn "%d" 102
        ctx.print.s "test102"
        //equation: (x)
        let s = ((x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p)
            z2 <== (x)
            wr.tt <| (I 102)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 103
        ctx.print.s "test103"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 103)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 104
        ctx.print.s "test104"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 104)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 105
        ctx.print.s "test105"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 105)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 106
        ctx.print.s "test106"
        //equation: (-4.6)
        //printfn "%d" 107
        ctx.print.s "test107"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 107)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 108
        ctx.print.s "test108"
        //equation: ((-5.4)/5.0+(y/3.6-x)-3.4/0.6+y*x-((-1.0)+(-x)+1.1/(-4.8)/4.0)/((-7.6)+y/3.5*(-y)-(-x)))
        let s = (((-5.4)/5.0+(y/3.6-x)-3.4/0.6+y*x-((-1.0)+(-x)+1.1/(-4.8)/4.0)/((-7.6)+y/3.5*(-y)-(-x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-5.4)/5.0+(q/3.6-p)-3.4/0.6+q*p-((-1.0)+(-p)+1.1/(-4.8)/4.0)/((-7.6)+q/3.5*(-q)-(-p)))
            z2 <== ((-5.4)/5.0+(y/3.6-x)-3.4/0.6+y*x-((-1.0)+(-x)+1.1/(-4.8)/4.0)/((-7.6)+y/3.5*(-y)-(-x)))
            wr.tt <| (I 108)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 109
        ctx.print.s "test109"
        //equation: ((1.7+(-6.8))*y+((8.7-7.0)*((-6.6)/8.7-(-x)/y/y)*(-1.1)+(-1.2))-(((-7.8))-(-y)+x+(3.4/(-1.5)/(-y)-(-y))+(-7.5)/3.1)*((-6.6)+(-y)-(8.1/(-7.1))))
        let s = (((1.7+(-6.8))*y+((8.7-7.0)*((-6.6)/8.7-(-x)/y/y)*(-1.1)+(-1.2))-(((-7.8))-(-y)+x+(3.4/(-1.5)/(-y)-(-y))+(-7.5)/3.1)*((-6.6)+(-y)-(8.1/(-7.1))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((1.7+(-6.8))*q+((8.7-7.0)*((-6.6)/8.7-(-p)/q/q)*(-1.1)+(-1.2))-(((-7.8))-(-q)+p+(3.4/(-1.5)/(-q)-(-q))+(-7.5)/3.1)*((-6.6)+(-q)-(8.1/(-7.1))))
            z2 <== ((1.7+(-6.8))*y+((8.7-7.0)*((-6.6)/8.7-(-x)/y/y)*(-1.1)+(-1.2))-(((-7.8))-(-y)+x+(3.4/(-1.5)/(-y)-(-y))+(-7.5)/3.1)*((-6.6)+(-y)-(8.1/(-7.1))))
            wr.tt <| (I 109)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 110
        ctx.print.s "test110"
        //equation: (-3.4)
        //printfn "%d" 111
        ctx.print.s "test111"
        //equation: ((-y)/0.3/6.0)
        let s = (((-y)/0.3/6.0)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)/0.3/6.0)
            z2 <== ((-y)/0.3/6.0)
            wr.tt <| (I 111)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 112
        ctx.print.s "test112"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 112)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 113
        ctx.print.s "test113"
        //equation: (2.4)
        //printfn "%d" 114
        ctx.print.s "test114"
        //equation: ((((-y)/(-y)/x)*((-5.0)-y/(-x)-(-y))+y-6.6-3.1)/(8.6+8.5*y+(-1.2)*5.6)-((-8.0)-(7.6*4.5)))
        let s = (((((-y)/(-y)/x)*((-5.0)-y/(-x)-(-y))+y-6.6-3.1)/(8.6+8.5*y+(-1.2)*5.6)-((-8.0)-(7.6*4.5)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-q)/(-q)/p)*((-5.0)-q/(-p)-(-q))+q-6.6-3.1)/(8.6+8.5*q+(-1.2)*5.6)-((-8.0)-(7.6*4.5)))
            z2 <== ((((-y)/(-y)/x)*((-5.0)-y/(-x)-(-y))+y-6.6-3.1)/(8.6+8.5*y+(-1.2)*5.6)-((-8.0)-(7.6*4.5)))
            wr.tt <| (I 114)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 115
        ctx.print.s "test115"
        //equation: ((-6.0)-(((-y)))/y*((-6.1))*(-x)*2.2*((x-1.1+(-y)+(-y))+(-y)*(7.2))*(-0.7))
        let s = (((-6.0)-(((-y)))/y*((-6.1))*(-x)*2.2*((x-1.1+(-y)+(-y))+(-y)*(7.2))*(-0.7))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-6.0)-(((-q)))/q*((-6.1))*(-p)*2.2*((p-1.1+(-q)+(-q))+(-q)*(7.2))*(-0.7))
            z2 <== ((-6.0)-(((-y)))/y*((-6.1))*(-x)*2.2*((x-1.1+(-y)+(-y))+(-y)*(7.2))*(-0.7))
            wr.tt <| (I 115)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 116
        ctx.print.s "test116"
        //equation: (y+4.0-((-2.7)/((-7.3)/1.2)-((-y)*(-y))-((-y)/7.0*y)*((-x)*4.0/8.2+(-3.4)-(-7.4)))/7.2*(0.4+(x)))
        let s = ((y+4.0-((-2.7)/((-7.3)/1.2)-((-y)*(-y))-((-y)/7.0*y)*((-x)*4.0/8.2+(-3.4)-(-7.4)))/7.2*(0.4+(x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q+4.0-((-2.7)/((-7.3)/1.2)-((-q)*(-q))-((-q)/7.0*q)*((-p)*4.0/8.2+(-3.4)-(-7.4)))/7.2*(0.4+(p)))
            z2 <== (y+4.0-((-2.7)/((-7.3)/1.2)-((-y)*(-y))-((-y)/7.0*y)*((-x)*4.0/8.2+(-3.4)-(-7.4)))/7.2*(0.4+(x)))
            wr.tt <| (I 116)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 117
        ctx.print.s "test117"
        //equation: (0.0/(-x))
        let s = ((0.0/(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (0.0/(-p))
            z2 <== (0.0/(-x))
            wr.tt <| (I 117)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 118
        ctx.print.s "test118"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 118)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 119
        ctx.print.s "test119"
        //equation: ((((-x)/(-y)+y-y/7.0))+(y-((-8.1))/x))
        let s = (((((-x)/(-y)+y-y/7.0))+(y-((-8.1))/x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-p)/(-q)+q-q/7.0))+(q-((-8.1))/p))
            z2 <== ((((-x)/(-y)+y-y/7.0))+(y-((-8.1))/x))
            wr.tt <| (I 119)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 120
        ctx.print.s "test120"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 120)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 121
        ctx.print.s "test121"
        //equation: 2.6
        //printfn "%d" 122
        ctx.print.s "test122"
        //equation: ((-0.5)*x*y)-((-x)/y)/((-x)/(-y))
        let s = (((-0.5)*x*y)-((-x)/y)/((-x)/(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-0.5)*p*q)-((-p)/q)/((-p)/(-q))
            z2 <== ((-0.5)*x*y)-((-x)/y)/((-x)/(-y))
            wr.tt <| (I 122)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 123
        ctx.print.s "test123"
        //equation: (x/(2.6*(-3.6)+((-y)/6.2*(-y)*(-y)*(-x)))/(x/(-3.7)/(-y)/((-8.0)-4.3+(-1.0)*(-2.2))*(-x)))
        let s = ((x/(2.6*(-3.6)+((-y)/6.2*(-y)*(-y)*(-x)))/(x/(-3.7)/(-y)/((-8.0)-4.3+(-1.0)*(-2.2))*(-x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p/(2.6*(-3.6)+((-q)/6.2*(-q)*(-q)*(-p)))/(p/(-3.7)/(-q)/((-8.0)-4.3+(-1.0)*(-2.2))*(-p)))
            z2 <== (x/(2.6*(-3.6)+((-y)/6.2*(-y)*(-y)*(-x)))/(x/(-3.7)/(-y)/((-8.0)-4.3+(-1.0)*(-2.2))*(-x)))
            wr.tt <| (I 123)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 124
        ctx.print.s "test124"
        //equation: ((y+7.6*(-5.5)/(-2.2))*((-y)*6.8*(-3.3)*8.7)/(-1.5))-1.1-x*((-5.6)/y-((-2.6)-1.5*y-0.5+(-4.2))*y)+y
        let s = (((y+7.6*(-5.5)/(-2.2))*((-y)*6.8*(-3.3)*8.7)/(-1.5))-1.1-x*((-5.6)/y-((-2.6)-1.5*y-0.5+(-4.2))*y)+y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q+7.6*(-5.5)/(-2.2))*((-q)*6.8*(-3.3)*8.7)/(-1.5))-1.1-p*((-5.6)/q-((-2.6)-1.5*q-0.5+(-4.2))*q)+q
            z2 <== ((y+7.6*(-5.5)/(-2.2))*((-y)*6.8*(-3.3)*8.7)/(-1.5))-1.1-x*((-5.6)/y-((-2.6)-1.5*y-0.5+(-4.2))*y)+y
            wr.tt <| (I 124)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 125
        ctx.print.s "test125"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 125)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 126
        ctx.print.s "test126"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 126)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 127
        ctx.print.s "test127"
        //equation: (-5.4)
        //printfn "%d" 128
        ctx.print.s "test128"
        //equation: (-0.8)
        //printfn "%d" 129
        ctx.print.s "test129"
        //equation: (((-0.8)*6.1+(-y)/(-x)*8.5))
        let s = ((((-0.8)*6.1+(-y)/(-x)*8.5))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-0.8)*6.1+(-q)/(-p)*8.5))
            z2 <== (((-0.8)*6.1+(-y)/(-x)*8.5))
            wr.tt <| (I 129)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 130
        ctx.print.s "test130"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 130)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 131
        ctx.print.s "test131"
        //equation: (-6.6)
        //printfn "%d" 132
        ctx.print.s "test132"
        //equation: ((((-x)/(-3.7)-(-5.3)+(-5.5))*((-6.3)-(-0.8))/(y)-8.2+5.7/7.2+((-x)))*(x-(y/0.3-5.5-(-7.7))/(6.0))-(((-y)-(-x)+(-x))*(-y)*(-7.7)))
        let s = (((((-x)/(-3.7)-(-5.3)+(-5.5))*((-6.3)-(-0.8))/(y)-8.2+5.7/7.2+((-x)))*(x-(y/0.3-5.5-(-7.7))/(6.0))-(((-y)-(-x)+(-x))*(-y)*(-7.7)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-p)/(-3.7)-(-5.3)+(-5.5))*((-6.3)-(-0.8))/(q)-8.2+5.7/7.2+((-p)))*(p-(q/0.3-5.5-(-7.7))/(6.0))-(((-q)-(-p)+(-p))*(-q)*(-7.7)))
            z2 <== ((((-x)/(-3.7)-(-5.3)+(-5.5))*((-6.3)-(-0.8))/(y)-8.2+5.7/7.2+((-x)))*(x-(y/0.3-5.5-(-7.7))/(6.0))-(((-y)-(-x)+(-x))*(-y)*(-7.7)))
            wr.tt <| (I 132)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 133
        ctx.print.s "test133"
        //equation: ((6.0+(-x)/(-1.2)/(-y)))
        let s = (((6.0+(-x)/(-1.2)/(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((6.0+(-p)/(-1.2)/(-q)))
            z2 <== ((6.0+(-x)/(-1.2)/(-y)))
            wr.tt <| (I 133)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 134
        ctx.print.s "test134"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 134)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 135
        ctx.print.s "test135"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 135)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 136
        ctx.print.s "test136"
        //equation: ((-7.4)-((-0.3)+((-x))*(y*2.6*(-x)+(-x)-(-x)))*(-x))
        let s = (((-7.4)-((-0.3)+((-x))*(y*2.6*(-x)+(-x)-(-x)))*(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-7.4)-((-0.3)+((-p))*(q*2.6*(-p)+(-p)-(-p)))*(-p))
            z2 <== ((-7.4)-((-0.3)+((-x))*(y*2.6*(-x)+(-x)-(-x)))*(-x))
            wr.tt <| (I 136)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 137
        ctx.print.s "test137"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 137)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 138
        ctx.print.s "test138"
        //equation: 4.4
        //printfn "%d" 139
        ctx.print.s "test139"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 139)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 140
        ctx.print.s "test140"
        //equation: ((-y)-(((-x)/1.6/y)-(-y)-(-y))+3.5/((-8.5)/(-2.7)/4.7)*x)
        let s = (((-y)-(((-x)/1.6/y)-(-y)-(-y))+3.5/((-8.5)/(-2.7)/4.7)*x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)-(((-p)/1.6/q)-(-q)-(-q))+3.5/((-8.5)/(-2.7)/4.7)*p)
            z2 <== ((-y)-(((-x)/1.6/y)-(-y)-(-y))+3.5/((-8.5)/(-2.7)/4.7)*x)
            wr.tt <| (I 140)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 141
        ctx.print.s "test141"
        //equation: ((-y)-2.0/x+(x+(-x)*y))
        let s = (((-y)-2.0/x+(x+(-x)*y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)-2.0/p+(p+(-p)*q))
            z2 <== ((-y)-2.0/x+(x+(-x)*y))
            wr.tt <| (I 141)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 142
        ctx.print.s "test142"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 142)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 143
        ctx.print.s "test143"
        //equation: ((((-y)-1.5/6.0/8.4)))
        let s = (((((-y)-1.5/6.0/8.4)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-q)-1.5/6.0/8.4)))
            z2 <== ((((-y)-1.5/6.0/8.4)))
            wr.tt <| (I 143)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 144
        ctx.print.s "test144"
        //equation: (-6.4)
        //printfn "%d" 145
        ctx.print.s "test145"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 145)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 146
        ctx.print.s "test146"
        //equation: (7.4-((-1.3)*0.0*(-x)-(-y)-(-y))/1.0*x/(((-y)+(-8.5)+(-5.6)*6.0)*((-7.3)*(-y)*(-x)/y-(-7.8))*2.3+x*((-y)-2.8+(-2.7)+y))*y-(0.5+x*((-5.8)*5.0-(-3.0)+0.0)+((-x)/y-7.3-(-6.2)*(-0.0))+((-x))))
        let s = ((7.4-((-1.3)*0.0*(-x)-(-y)-(-y))/1.0*x/(((-y)+(-8.5)+(-5.6)*6.0)*((-7.3)*(-y)*(-x)/y-(-7.8))*2.3+x*((-y)-2.8+(-2.7)+y))*y-(0.5+x*((-5.8)*5.0-(-3.0)+0.0)+((-x)/y-7.3-(-6.2)*(-0.0))+((-x))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (7.4-((-1.3)*0.0*(-p)-(-q)-(-q))/1.0*p/(((-q)+(-8.5)+(-5.6)*6.0)*((-7.3)*(-q)*(-p)/q-(-7.8))*2.3+p*((-q)-2.8+(-2.7)+q))*q-(0.5+p*((-5.8)*5.0-(-3.0)+0.0)+((-p)/q-7.3-(-6.2)*(-0.0))+((-p))))
            z2 <== (7.4-((-1.3)*0.0*(-x)-(-y)-(-y))/1.0*x/(((-y)+(-8.5)+(-5.6)*6.0)*((-7.3)*(-y)*(-x)/y-(-7.8))*2.3+x*((-y)-2.8+(-2.7)+y))*y-(0.5+x*((-5.8)*5.0-(-3.0)+0.0)+((-x)/y-7.3-(-6.2)*(-0.0))+((-x))))
            wr.tt <| (I 146)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 147
        ctx.print.s "test147"
        //equation: (y-(-x)/x*(-x)/(y*(-8.5)/2.1+(-1.8)/(-2.1))*0.7+((5.4/y*(-x)+x)/((-3.7)-(-y)*2.1+3.7*(-x))-(-y)/(-y)-2.3/8.6*3.1*(-y)/(-3.2)))
        let s = ((y-(-x)/x*(-x)/(y*(-8.5)/2.1+(-1.8)/(-2.1))*0.7+((5.4/y*(-x)+x)/((-3.7)-(-y)*2.1+3.7*(-x))-(-y)/(-y)-2.3/8.6*3.1*(-y)/(-3.2)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q-(-p)/p*(-p)/(q*(-8.5)/2.1+(-1.8)/(-2.1))*0.7+((5.4/q*(-p)+p)/((-3.7)-(-q)*2.1+3.7*(-p))-(-q)/(-q)-2.3/8.6*3.1*(-q)/(-3.2)))
            z2 <== (y-(-x)/x*(-x)/(y*(-8.5)/2.1+(-1.8)/(-2.1))*0.7+((5.4/y*(-x)+x)/((-3.7)-(-y)*2.1+3.7*(-x))-(-y)/(-y)-2.3/8.6*3.1*(-y)/(-3.2)))
            wr.tt <| (I 147)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 148
        ctx.print.s "test148"
        //equation: ((-7.5)/(-y)+y)
        let s = (((-7.5)/(-y)+y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-7.5)/(-q)+q)
            z2 <== ((-7.5)/(-y)+y)
            wr.tt <| (I 148)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 149
        ctx.print.s "test149"
        //equation: y/(-x)/(((-2.6)*(-x)-y)+(x/(-y)*x/y+(-x))-(-y))/6.2-((-5.7)*x)+((6.4)-(-y))
        let s = (y/(-x)/(((-2.6)*(-x)-y)+(x/(-y)*x/y+(-x))-(-y))/6.2-((-5.7)*x)+((6.4)-(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q/(-p)/(((-2.6)*(-p)-q)+(p/(-q)*p/q+(-p))-(-q))/6.2-((-5.7)*p)+((6.4)-(-q))
            z2 <== y/(-x)/(((-2.6)*(-x)-y)+(x/(-y)*x/y+(-x))-(-y))/6.2-((-5.7)*x)+((6.4)-(-y))
            wr.tt <| (I 149)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 150
        ctx.print.s "test150"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 150)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 151
        ctx.print.s "test151"
        //equation: (((-5.4)+2.7)*(y/(-2.7)+7.1)-(y/((-x)/y*(-5.6)+(-x)+(-x))*((-x)+(-x)*(-0.4)+2.7)-((-x)*x*(-3.1)*7.7)+((-y)*(-6.0)))/(-y)-((-3.8)/(-5.3)/y)+((-4.5))-(0.2-y)*((-6.4)*3.3+(-8.3))+8.8)
        let s = ((((-5.4)+2.7)*(y/(-2.7)+7.1)-(y/((-x)/y*(-5.6)+(-x)+(-x))*((-x)+(-x)*(-0.4)+2.7)-((-x)*x*(-3.1)*7.7)+((-y)*(-6.0)))/(-y)-((-3.8)/(-5.3)/y)+((-4.5))-(0.2-y)*((-6.4)*3.3+(-8.3))+8.8)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-5.4)+2.7)*(q/(-2.7)+7.1)-(q/((-p)/q*(-5.6)+(-p)+(-p))*((-p)+(-p)*(-0.4)+2.7)-((-p)*p*(-3.1)*7.7)+((-q)*(-6.0)))/(-q)-((-3.8)/(-5.3)/q)+((-4.5))-(0.2-q)*((-6.4)*3.3+(-8.3))+8.8)
            z2 <== (((-5.4)+2.7)*(y/(-2.7)+7.1)-(y/((-x)/y*(-5.6)+(-x)+(-x))*((-x)+(-x)*(-0.4)+2.7)-((-x)*x*(-3.1)*7.7)+((-y)*(-6.0)))/(-y)-((-3.8)/(-5.3)/y)+((-4.5))-(0.2-y)*((-6.4)*3.3+(-8.3))+8.8)
            wr.tt <| (I 151)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 152
        ctx.print.s "test152"
        //equation: (6.7*(((-x)+(-y)+y-(-y))*(-x)-((-7.4))*(y))-4.6/((8.2+(-6.0)+y-(-y)/y)-3.8/(-x)-(-x))*x)
        let s = ((6.7*(((-x)+(-y)+y-(-y))*(-x)-((-7.4))*(y))-4.6/((8.2+(-6.0)+y-(-y)/y)-3.8/(-x)-(-x))*x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (6.7*(((-p)+(-q)+q-(-q))*(-p)-((-7.4))*(q))-4.6/((8.2+(-6.0)+q-(-q)/q)-3.8/(-p)-(-p))*p)
            z2 <== (6.7*(((-x)+(-y)+y-(-y))*(-x)-((-7.4))*(y))-4.6/((8.2+(-6.0)+y-(-y)/y)-3.8/(-x)-(-x))*x)
            wr.tt <| (I 152)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 153
        ctx.print.s "test153"
        //equation: (-2.7)
        //printfn "%d" 154
        ctx.print.s "test154"
        //equation: x/(-y)
        let s = (x/(-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p/(-q)
            z2 <== x/(-y)
            wr.tt <| (I 154)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 155
        ctx.print.s "test155"
        //equation: ((8.1*((-y)+6.5*(-0.6)+(-8.5)))+(4.8*((-8.6)*(-7.3)/y/8.1+(-3.7))-(-y)+((-3.2)+2.1+7.7-y-(-6.1)))+y)
        let s = (((8.1*((-y)+6.5*(-0.6)+(-8.5)))+(4.8*((-8.6)*(-7.3)/y/8.1+(-3.7))-(-y)+((-3.2)+2.1+7.7-y-(-6.1)))+y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((8.1*((-q)+6.5*(-0.6)+(-8.5)))+(4.8*((-8.6)*(-7.3)/q/8.1+(-3.7))-(-q)+((-3.2)+2.1+7.7-q-(-6.1)))+q)
            z2 <== ((8.1*((-y)+6.5*(-0.6)+(-8.5)))+(4.8*((-8.6)*(-7.3)/y/8.1+(-3.7))-(-y)+((-3.2)+2.1+7.7-y-(-6.1)))+y)
            wr.tt <| (I 155)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 156
        ctx.print.s "test156"
        //equation: (0.4+(-6.8)/((x*0.5))+(((-2.3)+(-x)-(-y)+x-(-5.3))/0.4))
        let s = ((0.4+(-6.8)/((x*0.5))+(((-2.3)+(-x)-(-y)+x-(-5.3))/0.4))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (0.4+(-6.8)/((p*0.5))+(((-2.3)+(-p)-(-q)+p-(-5.3))/0.4))
            z2 <== (0.4+(-6.8)/((x*0.5))+(((-2.3)+(-x)-(-y)+x-(-5.3))/0.4))
            wr.tt <| (I 156)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 157
        ctx.print.s "test157"
        //equation: 3.2
        //printfn "%d" 158
        ctx.print.s "test158"
        //equation: ((-8.8))
        //printfn "%d" 159
        ctx.print.s "test159"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 159)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 160
        ctx.print.s "test160"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 160)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 161
        ctx.print.s "test161"
        //equation: ((-y)/(0.6*(-0.0)-y+y+(-5.3))-y+(-2.3))+(-x)/((8.1+(-6.6)+(-x)/6.5/(-y)))*y*((y)*(-y)/(-x)/(-2.6)-((-y)*(-2.2)/x))
        let s = (((-y)/(0.6*(-0.0)-y+y+(-5.3))-y+(-2.3))+(-x)/((8.1+(-6.6)+(-x)/6.5/(-y)))*y*((y)*(-y)/(-x)/(-2.6)-((-y)*(-2.2)/x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)/(0.6*(-0.0)-q+q+(-5.3))-q+(-2.3))+(-p)/((8.1+(-6.6)+(-p)/6.5/(-q)))*q*((q)*(-q)/(-p)/(-2.6)-((-q)*(-2.2)/p))
            z2 <== ((-y)/(0.6*(-0.0)-y+y+(-5.3))-y+(-2.3))+(-x)/((8.1+(-6.6)+(-x)/6.5/(-y)))*y*((y)*(-y)/(-x)/(-2.6)-((-y)*(-2.2)/x))
            wr.tt <| (I 161)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 162
        ctx.print.s "test162"
        //equation: (-0.5)
        //printfn "%d" 163
        ctx.print.s "test163"
        //equation: (8.4-(((-x))*(-x)*x+((-y)/y))-(((-y)-(-0.6))/((-y)/(-y)-8.0)/((-y)+(-x))-(x)-(-x))+(x*(-7.1)*x/x/1.8/y-2.6/x))
        let s = ((8.4-(((-x))*(-x)*x+((-y)/y))-(((-y)-(-0.6))/((-y)/(-y)-8.0)/((-y)+(-x))-(x)-(-x))+(x*(-7.1)*x/x/1.8/y-2.6/x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (8.4-(((-p))*(-p)*p+((-q)/q))-(((-q)-(-0.6))/((-q)/(-q)-8.0)/((-q)+(-p))-(p)-(-p))+(p*(-7.1)*p/p/1.8/q-2.6/p))
            z2 <== (8.4-(((-x))*(-x)*x+((-y)/y))-(((-y)-(-0.6))/((-y)/(-y)-8.0)/((-y)+(-x))-(x)-(-x))+(x*(-7.1)*x/x/1.8/y-2.6/x))
            wr.tt <| (I 163)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 164
        ctx.print.s "test164"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 164)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 165
        ctx.print.s "test165"
        //equation: (-3.2)
        //printfn "%d" 166
        ctx.print.s "test166"
        //equation: ((-6.7)+(-x)-(((-x)*(-x)+x)+y))
        let s = (((-6.7)+(-x)-(((-x)*(-x)+x)+y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-6.7)+(-p)-(((-p)*(-p)+p)+q))
            z2 <== ((-6.7)+(-x)-(((-x)*(-x)+x)+y))
            wr.tt <| (I 166)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 167
        ctx.print.s "test167"
        //equation: ((y)+(y+7.5*(-6.8)/(-8.7))+(-4.8)*(x+6.5/0.8-(-x))*3.0)
        let s = (((y)+(y+7.5*(-6.8)/(-8.7))+(-4.8)*(x+6.5/0.8-(-x))*3.0)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q)+(q+7.5*(-6.8)/(-8.7))+(-4.8)*(p+6.5/0.8-(-p))*3.0)
            z2 <== ((y)+(y+7.5*(-6.8)/(-8.7))+(-4.8)*(x+6.5/0.8-(-x))*3.0)
            wr.tt <| (I 167)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 168
        ctx.print.s "test168"
        //equation: (7.5-x*(-y)*x+(-8.3)/(-y)*y+y)
        let s = ((7.5-x*(-y)*x+(-8.3)/(-y)*y+y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (7.5-p*(-q)*p+(-8.3)/(-q)*q+q)
            z2 <== (7.5-x*(-y)*x+(-8.3)/(-y)*y+y)
            wr.tt <| (I 168)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 169
        ctx.print.s "test169"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 169)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 170
        ctx.print.s "test170"
        //equation: (-0.2)-(-x)+(-x)*(6.3*((-x))+(y*(-x))-(8.3+(-y)/(-y))*(0.8))+(-3.0)
        let s = ((-0.2)-(-x)+(-x)*(6.3*((-x))+(y*(-x))-(8.3+(-y)/(-y))*(0.8))+(-3.0)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-0.2)-(-p)+(-p)*(6.3*((-p))+(q*(-p))-(8.3+(-q)/(-q))*(0.8))+(-3.0)
            z2 <== (-0.2)-(-x)+(-x)*(6.3*((-x))+(y*(-x))-(8.3+(-y)/(-y))*(0.8))+(-3.0)
            wr.tt <| (I 170)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 171
        ctx.print.s "test171"
        //equation: 8.8
        //printfn "%d" 172
        ctx.print.s "test172"
        //equation: (((-x)))*((-x)*4.5-((-x)*y*7.4+2.4/(-6.8)))/(-4.2)*2.0/(-y)/0.6+(-y)/(5.6)*(y/(-y)/6.0+0.6)/(-3.2)*(((-4.8)))
        let s = ((((-x)))*((-x)*4.5-((-x)*y*7.4+2.4/(-6.8)))/(-4.2)*2.0/(-y)/0.6+(-y)/(5.6)*(y/(-y)/6.0+0.6)/(-3.2)*(((-4.8)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-p)))*((-p)*4.5-((-p)*q*7.4+2.4/(-6.8)))/(-4.2)*2.0/(-q)/0.6+(-q)/(5.6)*(q/(-q)/6.0+0.6)/(-3.2)*(((-4.8)))
            z2 <== (((-x)))*((-x)*4.5-((-x)*y*7.4+2.4/(-6.8)))/(-4.2)*2.0/(-y)/0.6+(-y)/(5.6)*(y/(-y)/6.0+0.6)/(-3.2)*(((-4.8)))
            wr.tt <| (I 172)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 173
        ctx.print.s "test173"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 173)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 174
        ctx.print.s "test174"
        //equation: (-7.7)
        //printfn "%d" 175
        ctx.print.s "test175"
        //equation: ((-8.3))
        //printfn "%d" 176
        ctx.print.s "test176"
        //equation: ((-y)/(-0.0)/(-x))
        let s = (((-y)/(-0.0)/(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)/(-0.0)/(-p))
            z2 <== ((-y)/(-0.0)/(-x))
            wr.tt <| (I 176)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 177
        ctx.print.s "test177"
        //equation: (4.1-(-0.8)*(-y)-(-0.7)/(-8.8))-(-y)-y+x+y*(-x)+(y*3.6/y)+(-y)+((6.1)+(-y)-x-((-y)/(-x)-(-x)-(-8.8))/((-5.5)+(-y)))
        let s = ((4.1-(-0.8)*(-y)-(-0.7)/(-8.8))-(-y)-y+x+y*(-x)+(y*3.6/y)+(-y)+((6.1)+(-y)-x-((-y)/(-x)-(-x)-(-8.8))/((-5.5)+(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (4.1-(-0.8)*(-q)-(-0.7)/(-8.8))-(-q)-q+p+q*(-p)+(q*3.6/q)+(-q)+((6.1)+(-q)-p-((-q)/(-p)-(-p)-(-8.8))/((-5.5)+(-q)))
            z2 <== (4.1-(-0.8)*(-y)-(-0.7)/(-8.8))-(-y)-y+x+y*(-x)+(y*3.6/y)+(-y)+((6.1)+(-y)-x-((-y)/(-x)-(-x)-(-8.8))/((-5.5)+(-y)))
            wr.tt <| (I 177)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 178
        ctx.print.s "test178"
        //equation: (2.7/(x/0.4+x+(-y)/x))/6.3*(3.6/(-5.4)-x+2.3*x/3.4+((-y)/(-8.5)+(-7.3))/(-x))*(-x)
        let s = ((2.7/(x/0.4+x+(-y)/x))/6.3*(3.6/(-5.4)-x+2.3*x/3.4+((-y)/(-8.5)+(-7.3))/(-x))*(-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (2.7/(p/0.4+p+(-q)/p))/6.3*(3.6/(-5.4)-p+2.3*p/3.4+((-q)/(-8.5)+(-7.3))/(-p))*(-p)
            z2 <== (2.7/(x/0.4+x+(-y)/x))/6.3*(3.6/(-5.4)-x+2.3*x/3.4+((-y)/(-8.5)+(-7.3))/(-x))*(-x)
            wr.tt <| (I 178)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 179
        ctx.print.s "test179"
        //equation: ((y+(-0.2)*(3.4-(-y)+0.1+y)*((-x))/(x+(-x)*(-x)/x))/x)
        let s = (((y+(-0.2)*(3.4-(-y)+0.1+y)*((-x))/(x+(-x)*(-x)/x))/x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q+(-0.2)*(3.4-(-q)+0.1+q)*((-p))/(p+(-p)*(-p)/p))/p)
            z2 <== ((y+(-0.2)*(3.4-(-y)+0.1+y)*((-x))/(x+(-x)*(-x)/x))/x)
            wr.tt <| (I 179)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 180
        ctx.print.s "test180"
        //equation: 8.0
        //printfn "%d" 181
        ctx.print.s "test181"
        //equation: ((x+3.6*y*(-x))*((-2.5)/8.7+(-y))-(-0.0)*2.3-(-y)*(-1.0)/x)
        let s = (((x+3.6*y*(-x))*((-2.5)/8.7+(-y))-(-0.0)*2.3-(-y)*(-1.0)/x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p+3.6*q*(-p))*((-2.5)/8.7+(-q))-(-0.0)*2.3-(-q)*(-1.0)/p)
            z2 <== ((x+3.6*y*(-x))*((-2.5)/8.7+(-y))-(-0.0)*2.3-(-y)*(-1.0)/x)
            wr.tt <| (I 181)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 182
        ctx.print.s "test182"
        //equation: (-3.5)
        //printfn "%d" 183
        ctx.print.s "test183"
        //equation: (y*((-1.4)-7.0/(-8.7))*((x+(-x)*x*x+(-y))-(-6.7)*((-1.0)+(-x)-(-2.1))*(x))/((8.2*x/x-(-7.6))-((-5.7)/(-y))*y-y+((-y)-(-x)/y)))
        let s = ((y*((-1.4)-7.0/(-8.7))*((x+(-x)*x*x+(-y))-(-6.7)*((-1.0)+(-x)-(-2.1))*(x))/((8.2*x/x-(-7.6))-((-5.7)/(-y))*y-y+((-y)-(-x)/y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q*((-1.4)-7.0/(-8.7))*((p+(-p)*p*p+(-q))-(-6.7)*((-1.0)+(-p)-(-2.1))*(p))/((8.2*p/p-(-7.6))-((-5.7)/(-q))*q-q+((-q)-(-p)/q)))
            z2 <== (y*((-1.4)-7.0/(-8.7))*((x+(-x)*x*x+(-y))-(-6.7)*((-1.0)+(-x)-(-2.1))*(x))/((8.2*x/x-(-7.6))-((-5.7)/(-y))*y-y+((-y)-(-x)/y)))
            wr.tt <| (I 183)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 184
        ctx.print.s "test184"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 184)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 185
        ctx.print.s "test185"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 185)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 186
        ctx.print.s "test186"
        //equation: (-4.8)*y/((3.0)*(-8.3)+(y+x+(-x)-8.1))
        let s = ((-4.8)*y/((3.0)*(-8.3)+(y+x+(-x)-8.1))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-4.8)*q/((3.0)*(-8.3)+(q+p+(-p)-8.1))
            z2 <== (-4.8)*y/((3.0)*(-8.3)+(y+x+(-x)-8.1))
            wr.tt <| (I 186)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 187
        ctx.print.s "test187"
        //equation: (-8.2)
        //printfn "%d" 188
        ctx.print.s "test188"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 188)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 189
        ctx.print.s "test189"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 189)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 190
        ctx.print.s "test190"
        //equation: (y+((y+y-x*x*(-y))-(-x)+((-0.8)/(-x)/5.0*2.0*3.7)/(-4.0)))
        let s = ((y+((y+y-x*x*(-y))-(-x)+((-0.8)/(-x)/5.0*2.0*3.7)/(-4.0)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q+((q+q-p*p*(-q))-(-p)+((-0.8)/(-p)/5.0*2.0*3.7)/(-4.0)))
            z2 <== (y+((y+y-x*x*(-y))-(-x)+((-0.8)/(-x)/5.0*2.0*3.7)/(-4.0)))
            wr.tt <| (I 190)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 191
        ctx.print.s "test191"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 191)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 192
        ctx.print.s "test192"
        //equation: (((7.6/0.1-(-y)*y)/5.4+((-5.5)))*(5.5-(0.7*y+8.3)*((-8.1))-(-x)*(-2.1)-((-8.4)*(-8.5)*(-8.0)))-(((-7.3)*8.8*(-x)/(-6.2))))
        let s = ((((7.6/0.1-(-y)*y)/5.4+((-5.5)))*(5.5-(0.7*y+8.3)*((-8.1))-(-x)*(-2.1)-((-8.4)*(-8.5)*(-8.0)))-(((-7.3)*8.8*(-x)/(-6.2))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((7.6/0.1-(-q)*q)/5.4+((-5.5)))*(5.5-(0.7*q+8.3)*((-8.1))-(-p)*(-2.1)-((-8.4)*(-8.5)*(-8.0)))-(((-7.3)*8.8*(-p)/(-6.2))))
            z2 <== (((7.6/0.1-(-y)*y)/5.4+((-5.5)))*(5.5-(0.7*y+8.3)*((-8.1))-(-x)*(-2.1)-((-8.4)*(-8.5)*(-8.0)))-(((-7.3)*8.8*(-x)/(-6.2))))
            wr.tt <| (I 192)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 193
        ctx.print.s "test193"
        //equation: 3.2
        //printfn "%d" 194
        ctx.print.s "test194"
        //equation: (-8.5)
        //printfn "%d" 195
        ctx.print.s "test195"
        //equation: ((-7.8))
        //printfn "%d" 196
        ctx.print.s "test196"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 196)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 197
        ctx.print.s "test197"
        //equation: (0.6-((-6.3)*(5.0+(-6.7))+x*x/((-x)-(-x)/3.6+(-y)))+(-4.4)/((-7.4)/3.2+((-y)*(-4.5)/(-0.2)-x+(-y))))
        let s = ((0.6-((-6.3)*(5.0+(-6.7))+x*x/((-x)-(-x)/3.6+(-y)))+(-4.4)/((-7.4)/3.2+((-y)*(-4.5)/(-0.2)-x+(-y))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (0.6-((-6.3)*(5.0+(-6.7))+p*p/((-p)-(-p)/3.6+(-q)))+(-4.4)/((-7.4)/3.2+((-q)*(-4.5)/(-0.2)-p+(-q))))
            z2 <== (0.6-((-6.3)*(5.0+(-6.7))+x*x/((-x)-(-x)/3.6+(-y)))+(-4.4)/((-7.4)/3.2+((-y)*(-4.5)/(-0.2)-x+(-y))))
            wr.tt <| (I 197)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 198
        ctx.print.s "test198"
        //equation: 2.5
        //printfn "%d" 199
        ctx.print.s "test199"
        //equation: ((y-((-x)*(-y)))/(-x)-3.4-0.1)
        let s = (((y-((-x)*(-y)))/(-x)-3.4-0.1)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q-((-p)*(-q)))/(-p)-3.4-0.1)
            z2 <== ((y-((-x)*(-y)))/(-x)-3.4-0.1)
            wr.tt <| (I 199)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 200
        ctx.print.s "test200"
        //equation: (-8.0)
        //printfn "%d" 201
        ctx.print.s "test201"
        //equation: (y+y*(((-y)+(-6.2)+(-2.7)*(-y)*(-1.2)))*((7.0/(-3.4))*y*((-4.4)/(-2.6)/(-x)+2.0*y)))
        let s = ((y+y*(((-y)+(-6.2)+(-2.7)*(-y)*(-1.2)))*((7.0/(-3.4))*y*((-4.4)/(-2.6)/(-x)+2.0*y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q+q*(((-q)+(-6.2)+(-2.7)*(-q)*(-1.2)))*((7.0/(-3.4))*q*((-4.4)/(-2.6)/(-p)+2.0*q)))
            z2 <== (y+y*(((-y)+(-6.2)+(-2.7)*(-y)*(-1.2)))*((7.0/(-3.4))*y*((-4.4)/(-2.6)/(-x)+2.0*y)))
            wr.tt <| (I 201)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 202
        ctx.print.s "test202"
        //equation: ((7.0/7.1*y+(-x)*x+(-8.2)*(-x))+7.2+(((-y))*(y-(-y)*(-x)+2.4)-(-y)/3.2-x)+x+4.1+y)
        let s = (((7.0/7.1*y+(-x)*x+(-8.2)*(-x))+7.2+(((-y))*(y-(-y)*(-x)+2.4)-(-y)/3.2-x)+x+4.1+y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((7.0/7.1*q+(-p)*p+(-8.2)*(-p))+7.2+(((-q))*(q-(-q)*(-p)+2.4)-(-q)/3.2-p)+p+4.1+q)
            z2 <== ((7.0/7.1*y+(-x)*x+(-8.2)*(-x))+7.2+(((-y))*(y-(-y)*(-x)+2.4)-(-y)/3.2-x)+x+4.1+y)
            wr.tt <| (I 202)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 203
        ctx.print.s "test203"
        //equation: (-7.2)
        //printfn "%d" 204
        ctx.print.s "test204"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 204)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 205
        ctx.print.s "test205"
        //equation: 0.4
        //printfn "%d" 206
        ctx.print.s "test206"
        //equation: (-0.3)
        //printfn "%d" 207
        ctx.print.s "test207"
        //equation: 4.5
        //printfn "%d" 208
        ctx.print.s "test208"
        //equation: ((((-y)-(-x)+(-x))+(x))/(1.4)+(6.7*x+(-8.3)/(-7.2)/(-0.1))+3.8-2.1+6.0+(((-y)-(-6.1)*(-8.1)/y+(-x))+(4.1/(-y)*5.4-x-(-4.7))/(y*(-1.0))+x)-((2.3+(-y)/(-y)*2.8)+2.5-(-8.7)*((-4.4)-0.3+2.8+7.0-6.0)-8.0))
        let s = (((((-y)-(-x)+(-x))+(x))/(1.4)+(6.7*x+(-8.3)/(-7.2)/(-0.1))+3.8-2.1+6.0+(((-y)-(-6.1)*(-8.1)/y+(-x))+(4.1/(-y)*5.4-x-(-4.7))/(y*(-1.0))+x)-((2.3+(-y)/(-y)*2.8)+2.5-(-8.7)*((-4.4)-0.3+2.8+7.0-6.0)-8.0))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-q)-(-p)+(-p))+(p))/(1.4)+(6.7*p+(-8.3)/(-7.2)/(-0.1))+3.8-2.1+6.0+(((-q)-(-6.1)*(-8.1)/q+(-p))+(4.1/(-q)*5.4-p-(-4.7))/(q*(-1.0))+p)-((2.3+(-q)/(-q)*2.8)+2.5-(-8.7)*((-4.4)-0.3+2.8+7.0-6.0)-8.0))
            z2 <== ((((-y)-(-x)+(-x))+(x))/(1.4)+(6.7*x+(-8.3)/(-7.2)/(-0.1))+3.8-2.1+6.0+(((-y)-(-6.1)*(-8.1)/y+(-x))+(4.1/(-y)*5.4-x-(-4.7))/(y*(-1.0))+x)-((2.3+(-y)/(-y)*2.8)+2.5-(-8.7)*((-4.4)-0.3+2.8+7.0-6.0)-8.0))
            wr.tt <| (I 208)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 209
        ctx.print.s "test209"
        //equation: (-3.8)
        //printfn "%d" 210
        ctx.print.s "test210"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 210)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 211
        ctx.print.s "test211"
        //equation: (((-4.3)/x-7.8+6.4/y)/(-7.4))
        let s = ((((-4.3)/x-7.8+6.4/y)/(-7.4))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-4.3)/p-7.8+6.4/q)/(-7.4))
            z2 <== (((-4.3)/x-7.8+6.4/y)/(-7.4))
            wr.tt <| (I 211)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 212
        ctx.print.s "test212"
        //equation: 5.4
        //printfn "%d" 213
        ctx.print.s "test213"
        //equation: (7.1-(-x)-x)
        let s = ((7.1-(-x)-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (7.1-(-p)-p)
            z2 <== (7.1-(-x)-x)
            wr.tt <| (I 213)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 214
        ctx.print.s "test214"
        //equation: (3.8*4.7*(((-6.5)/(-x))*((-5.1)-x*(-x))*(x-6.1*(-4.4)/(-6.0)+(-7.2))/(-6.4)*(-x))/y)
        let s = ((3.8*4.7*(((-6.5)/(-x))*((-5.1)-x*(-x))*(x-6.1*(-4.4)/(-6.0)+(-7.2))/(-6.4)*(-x))/y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (3.8*4.7*(((-6.5)/(-p))*((-5.1)-p*(-p))*(p-6.1*(-4.4)/(-6.0)+(-7.2))/(-6.4)*(-p))/q)
            z2 <== (3.8*4.7*(((-6.5)/(-x))*((-5.1)-x*(-x))*(x-6.1*(-4.4)/(-6.0)+(-7.2))/(-6.4)*(-x))/y)
            wr.tt <| (I 214)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 215
        ctx.print.s "test215"
        //equation: 6.7
        //printfn "%d" 216
        ctx.print.s "test216"
        //equation: (-7.2)
        //printfn "%d" 217
        ctx.print.s "test217"
        //equation: 5.5
        //printfn "%d" 218
        ctx.print.s "test218"
        //equation: (8.4+((-y)))
        let s = ((8.4+((-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (8.4+((-q)))
            z2 <== (8.4+((-y)))
            wr.tt <| (I 218)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 219
        ctx.print.s "test219"
        //equation: (8.2-(-8.3)*8.2/y)*x+(8.0+((-y)/(-x)/y*(-6.7)-x))
        let s = ((8.2-(-8.3)*8.2/y)*x+(8.0+((-y)/(-x)/y*(-6.7)-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (8.2-(-8.3)*8.2/q)*p+(8.0+((-q)/(-p)/q*(-6.7)-p))
            z2 <== (8.2-(-8.3)*8.2/y)*x+(8.0+((-y)/(-x)/y*(-6.7)-x))
            wr.tt <| (I 219)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 220
        ctx.print.s "test220"
        //equation: ((x)+x*x*((-x)*x/(-5.8)-(-6.5))-((-8.3)+(-6.2)/(-x)))
        let s = (((x)+x*x*((-x)*x/(-5.8)-(-6.5))-((-8.3)+(-6.2)/(-x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p)+p*p*((-p)*p/(-5.8)-(-6.5))-((-8.3)+(-6.2)/(-p)))
            z2 <== ((x)+x*x*((-x)*x/(-5.8)-(-6.5))-((-8.3)+(-6.2)/(-x)))
            wr.tt <| (I 220)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 221
        ctx.print.s "test221"
        //equation: (8.3*((x)))
        let s = ((8.3*((x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (8.3*((p)))
            z2 <== (8.3*((x)))
            wr.tt <| (I 221)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 222
        ctx.print.s "test222"
        //equation: ((-y)+((y+y+4.0+(-2.1)-(-5.7))/(6.1/(-0.7)/y+(-5.1))/(-0.6))-x)
        let s = (((-y)+((y+y+4.0+(-2.1)-(-5.7))/(6.1/(-0.7)/y+(-5.1))/(-0.6))-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)+((q+q+4.0+(-2.1)-(-5.7))/(6.1/(-0.7)/q+(-5.1))/(-0.6))-p)
            z2 <== ((-y)+((y+y+4.0+(-2.1)-(-5.7))/(6.1/(-0.7)/y+(-5.1))/(-0.6))-x)
            wr.tt <| (I 222)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 223
        ctx.print.s "test223"
        //equation: (5.7)
        //printfn "%d" 224
        ctx.print.s "test224"
        //equation: 2.5
        //printfn "%d" 225
        ctx.print.s "test225"
        //equation: (x*x)
        let s = ((x*x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p*p)
            z2 <== (x*x)
            wr.tt <| (I 225)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 226
        ctx.print.s "test226"
        //equation: (-2.0)
        //printfn "%d" 227
        ctx.print.s "test227"
        //equation: (-3.6)
        //printfn "%d" 228
        ctx.print.s "test228"
        //equation: 4.1
        //printfn "%d" 229
        ctx.print.s "test229"
        //equation: (((-7.7)+(-3.1)/7.2)+(-y))
        let s = ((((-7.7)+(-3.1)/7.2)+(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-7.7)+(-3.1)/7.2)+(-q))
            z2 <== (((-7.7)+(-3.1)/7.2)+(-y))
            wr.tt <| (I 229)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 230
        ctx.print.s "test230"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 230)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 231
        ctx.print.s "test231"
        //equation: 2.7
        //printfn "%d" 232
        ctx.print.s "test232"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 232)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 233
        ctx.print.s "test233"
        //equation: 4.8
        //printfn "%d" 234
        ctx.print.s "test234"
        //equation: (-7.6)
        //printfn "%d" 235
        ctx.print.s "test235"
        //equation: (-3.7)
        //printfn "%d" 236
        ctx.print.s "test236"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 236)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 237
        ctx.print.s "test237"
        //equation: (x*((x/(-y)-2.6/(-y)+y)/y))
        let s = ((x*((x/(-y)-2.6/(-y)+y)/y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p*((p/(-q)-2.6/(-q)+q)/q))
            z2 <== (x*((x/(-y)-2.6/(-y)+y)/y))
            wr.tt <| (I 237)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 238
        ctx.print.s "test238"
        //equation: ((-4.4)/((y*(-x)*y)/x+x+y*(-y)/(-x)/(-0.0))/(-1.2)*(((-5.6)/6.6)/((-x)-x/8.8)/x/5.8-x)+(((-y)-x/4.8)*((-x)*x)))
        let s = (((-4.4)/((y*(-x)*y)/x+x+y*(-y)/(-x)/(-0.0))/(-1.2)*(((-5.6)/6.6)/((-x)-x/8.8)/x/5.8-x)+(((-y)-x/4.8)*((-x)*x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-4.4)/((q*(-p)*q)/p+p+q*(-q)/(-p)/(-0.0))/(-1.2)*(((-5.6)/6.6)/((-p)-p/8.8)/p/5.8-p)+(((-q)-p/4.8)*((-p)*p)))
            z2 <== ((-4.4)/((y*(-x)*y)/x+x+y*(-y)/(-x)/(-0.0))/(-1.2)*(((-5.6)/6.6)/((-x)-x/8.8)/x/5.8-x)+(((-y)-x/4.8)*((-x)*x)))
            wr.tt <| (I 238)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 239
        ctx.print.s "test239"
        //equation: (0.6)
        //printfn "%d" 240
        ctx.print.s "test240"
        //equation: ((y)+((-6.5)+(-x)*(-y))*((x/(-7.7)/y*(-x)+(-x))+x/6.8)+(-y))
        let s = (((y)+((-6.5)+(-x)*(-y))*((x/(-7.7)/y*(-x)+(-x))+x/6.8)+(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q)+((-6.5)+(-p)*(-q))*((p/(-7.7)/q*(-p)+(-p))+p/6.8)+(-q))
            z2 <== ((y)+((-6.5)+(-x)*(-y))*((x/(-7.7)/y*(-x)+(-x))+x/6.8)+(-y))
            wr.tt <| (I 240)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 241
        ctx.print.s "test241"
        //equation: 6.2
        //printfn "%d" 242
        ctx.print.s "test242"
        //equation: (y*2.4)
        let s = ((y*2.4)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q*2.4)
            z2 <== (y*2.4)
            wr.tt <| (I 242)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 243
        ctx.print.s "test243"
        //equation: y*((x)-(x)-(-6.1))-((-x))-y
        let s = (y*((x)-(x)-(-6.1))-((-x))-y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q*((p)-(p)-(-6.1))-((-p))-q
            z2 <== y*((x)-(x)-(-6.1))-((-x))-y
            wr.tt <| (I 243)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 244
        ctx.print.s "test244"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 244)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 245
        ctx.print.s "test245"
        //equation: (((-6.1)/(-0.1)/y)+x*(-7.6)*8.2*0.1)-(-y)
        let s = ((((-6.1)/(-0.1)/y)+x*(-7.6)*8.2*0.1)-(-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-6.1)/(-0.1)/q)+p*(-7.6)*8.2*0.1)-(-q)
            z2 <== (((-6.1)/(-0.1)/y)+x*(-7.6)*8.2*0.1)-(-y)
            wr.tt <| (I 245)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 246
        ctx.print.s "test246"
        //equation: ((-x)/(-y))
        let s = (((-x)/(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)/(-q))
            z2 <== ((-x)/(-y))
            wr.tt <| (I 246)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 247
        ctx.print.s "test247"
        //equation: 1.5+4.0
        //printfn "%d" 248
        ctx.print.s "test248"
        //equation: ((0.5/((-8.7)/(-y)/6.4+0.8/0.4))*(-x)/(-x))
        let s = (((0.5/((-8.7)/(-y)/6.4+0.8/0.4))*(-x)/(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((0.5/((-8.7)/(-q)/6.4+0.8/0.4))*(-p)/(-p))
            z2 <== ((0.5/((-8.7)/(-y)/6.4+0.8/0.4))*(-x)/(-x))
            wr.tt <| (I 248)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 249
        ctx.print.s "test249"
        //equation: (x+((-7.0)+(0.7+(-3.0)/(-2.7)/(-y)-(-y))+((-0.3)/5.8)/6.5-x)*8.0)
        let s = ((x+((-7.0)+(0.7+(-3.0)/(-2.7)/(-y)-(-y))+((-0.3)/5.8)/6.5-x)*8.0)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p+((-7.0)+(0.7+(-3.0)/(-2.7)/(-q)-(-q))+((-0.3)/5.8)/6.5-p)*8.0)
            z2 <== (x+((-7.0)+(0.7+(-3.0)/(-2.7)/(-y)-(-y))+((-0.3)/5.8)/6.5-x)*8.0)
            wr.tt <| (I 249)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 250
        ctx.print.s "test250"
        //equation: (-8.7)
        //printfn "%d" 251
        ctx.print.s "test251"
        //equation: (((-2.3)/((-6.8)-(-y)+2.5+y)+6.7*(5.1+4.1-(-5.0)))*((-7.3)+y+3.4/(-x)+5.4-2.0*(-y)*3.2/y)+((-x)-x+8.6)-y)
        let s = ((((-2.3)/((-6.8)-(-y)+2.5+y)+6.7*(5.1+4.1-(-5.0)))*((-7.3)+y+3.4/(-x)+5.4-2.0*(-y)*3.2/y)+((-x)-x+8.6)-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-2.3)/((-6.8)-(-q)+2.5+q)+6.7*(5.1+4.1-(-5.0)))*((-7.3)+q+3.4/(-p)+5.4-2.0*(-q)*3.2/q)+((-p)-p+8.6)-q)
            z2 <== (((-2.3)/((-6.8)-(-y)+2.5+y)+6.7*(5.1+4.1-(-5.0)))*((-7.3)+y+3.4/(-x)+5.4-2.0*(-y)*3.2/y)+((-x)-x+8.6)-y)
            wr.tt <| (I 251)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 252
        ctx.print.s "test252"
        //equation: ((3.3)+(((-x)-(-6.3)))/(-5.5))
        let s = (((3.3)+(((-x)-(-6.3)))/(-5.5))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((3.3)+(((-p)-(-6.3)))/(-5.5))
            z2 <== ((3.3)+(((-x)-(-6.3)))/(-5.5))
            wr.tt <| (I 252)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 253
        ctx.print.s "test253"
        //equation: ((4.4-2.2-(-5.8)-7.0)-((-1.1)*(-y)-(-x)/(-3.8)/(-x))-6.4+(-7.2))/((5.2+3.6/(-4.2)*8.5/(-0.5)))+(-7.0)-(-5.2)+(y)+((-y))-((-y)-(-4.6)*x-(-x)-(-2.0))+(-0.6)
        let s = (((4.4-2.2-(-5.8)-7.0)-((-1.1)*(-y)-(-x)/(-3.8)/(-x))-6.4+(-7.2))/((5.2+3.6/(-4.2)*8.5/(-0.5)))+(-7.0)-(-5.2)+(y)+((-y))-((-y)-(-4.6)*x-(-x)-(-2.0))+(-0.6)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((4.4-2.2-(-5.8)-7.0)-((-1.1)*(-q)-(-p)/(-3.8)/(-p))-6.4+(-7.2))/((5.2+3.6/(-4.2)*8.5/(-0.5)))+(-7.0)-(-5.2)+(q)+((-q))-((-q)-(-4.6)*p-(-p)-(-2.0))+(-0.6)
            z2 <== ((4.4-2.2-(-5.8)-7.0)-((-1.1)*(-y)-(-x)/(-3.8)/(-x))-6.4+(-7.2))/((5.2+3.6/(-4.2)*8.5/(-0.5)))+(-7.0)-(-5.2)+(y)+((-y))-((-y)-(-4.6)*x-(-x)-(-2.0))+(-0.6)
            wr.tt <| (I 253)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 254
        ctx.print.s "test254"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 254)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 255
        ctx.print.s "test255"
        //equation: ((-x)+((-0.6)-8.6)+((-x)*y*(-4.2)*(-2.7))+1.8)+((x-(-y)+(-1.6)*6.0)+x*((-x)-0.5*(-6.6)))/(-5.5)
        let s = (((-x)+((-0.6)-8.6)+((-x)*y*(-4.2)*(-2.7))+1.8)+((x-(-y)+(-1.6)*6.0)+x*((-x)-0.5*(-6.6)))/(-5.5)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)+((-0.6)-8.6)+((-p)*q*(-4.2)*(-2.7))+1.8)+((p-(-q)+(-1.6)*6.0)+p*((-p)-0.5*(-6.6)))/(-5.5)
            z2 <== ((-x)+((-0.6)-8.6)+((-x)*y*(-4.2)*(-2.7))+1.8)+((x-(-y)+(-1.6)*6.0)+x*((-x)-0.5*(-6.6)))/(-5.5)
            wr.tt <| (I 255)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 256
        ctx.print.s "test256"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 256)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 257
        ctx.print.s "test257"
        //equation: 3.0/((-y)*(-0.1)/(-4.2)+(-y)/1.7)
        let s = (3.0/((-y)*(-0.1)/(-4.2)+(-y)/1.7)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== 3.0/((-q)*(-0.1)/(-4.2)+(-q)/1.7)
            z2 <== 3.0/((-y)*(-0.1)/(-4.2)+(-y)/1.7)
            wr.tt <| (I 257)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 258
        ctx.print.s "test258"
        //equation: ((((-4.7)*(-x)))/(-1.8))
        let s = (((((-4.7)*(-x)))/(-1.8))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-4.7)*(-p)))/(-1.8))
            z2 <== ((((-4.7)*(-x)))/(-1.8))
            wr.tt <| (I 258)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 259
        ctx.print.s "test259"
        //equation: ((x+1.4+(-6.3))+(-2.4)+((-y)/8.2/x*0.0*y)-(-4.4)+(-x))
        let s = (((x+1.4+(-6.3))+(-2.4)+((-y)/8.2/x*0.0*y)-(-4.4)+(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p+1.4+(-6.3))+(-2.4)+((-q)/8.2/p*0.0*q)-(-4.4)+(-p))
            z2 <== ((x+1.4+(-6.3))+(-2.4)+((-y)/8.2/x*0.0*y)-(-4.4)+(-x))
            wr.tt <| (I 259)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 260
        ctx.print.s "test260"
        //equation: y+(-x)*((1.0/x+1.0-(-y)*y)+(-2.2)+((-y)/(-1.3)+(-y))-(1.6*0.2-x+(-y)+y))
        let s = (y+(-x)*((1.0/x+1.0-(-y)*y)+(-2.2)+((-y)/(-1.3)+(-y))-(1.6*0.2-x+(-y)+y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q+(-p)*((1.0/p+1.0-(-q)*q)+(-2.2)+((-q)/(-1.3)+(-q))-(1.6*0.2-p+(-q)+q))
            z2 <== y+(-x)*((1.0/x+1.0-(-y)*y)+(-2.2)+((-y)/(-1.3)+(-y))-(1.6*0.2-x+(-y)+y))
            wr.tt <| (I 260)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 261
        ctx.print.s "test261"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 261)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 262
        ctx.print.s "test262"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 262)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 263
        ctx.print.s "test263"
        //equation: (((-7.6)*(x/x-(-y)*(-x))-(-2.0)+(-y)+(-y))+(-y)-(-7.0)/(-2.6)+(-3.6))
        let s = ((((-7.6)*(x/x-(-y)*(-x))-(-2.0)+(-y)+(-y))+(-y)-(-7.0)/(-2.6)+(-3.6))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-7.6)*(p/p-(-q)*(-p))-(-2.0)+(-q)+(-q))+(-q)-(-7.0)/(-2.6)+(-3.6))
            z2 <== (((-7.6)*(x/x-(-y)*(-x))-(-2.0)+(-y)+(-y))+(-y)-(-7.0)/(-2.6)+(-3.6))
            wr.tt <| (I 263)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 264
        ctx.print.s "test264"
        //equation: 8.6
        //printfn "%d" 265
        ctx.print.s "test265"
        //equation: 2.5
        //printfn "%d" 266
        ctx.print.s "test266"
        //equation: ((((-4.4)-1.5-x)*0.8)/y/0.7)
        let s = (((((-4.4)-1.5-x)*0.8)/y/0.7)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-4.4)-1.5-p)*0.8)/q/0.7)
            z2 <== ((((-4.4)-1.5-x)*0.8)/y/0.7)
            wr.tt <| (I 266)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 267
        ctx.print.s "test267"
        //equation: (-7.1)
        //printfn "%d" 268
        ctx.print.s "test268"
        //equation: (3.1/(2.6*(-y)/1.2)+(-8.7)*(-y))
        let s = ((3.1/(2.6*(-y)/1.2)+(-8.7)*(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (3.1/(2.6*(-q)/1.2)+(-8.7)*(-q))
            z2 <== (3.1/(2.6*(-y)/1.2)+(-8.7)*(-y))
            wr.tt <| (I 268)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 269
        ctx.print.s "test269"
        //equation: (((x+(-y)+(-y)/x)+x*(-y))/((y*(-3.3))-((-0.0)*(-x)/(-1.0)+x-8.3)*((-x))*(-y)+((-4.1)+0.4*(-2.8)))/((x/(-x)+(-x)*0.4*(-x))-y*(-x))-((y+(-7.0))*y+(4.7)-(-3.4)-(-y)))
        let s = ((((x+(-y)+(-y)/x)+x*(-y))/((y*(-3.3))-((-0.0)*(-x)/(-1.0)+x-8.3)*((-x))*(-y)+((-4.1)+0.4*(-2.8)))/((x/(-x)+(-x)*0.4*(-x))-y*(-x))-((y+(-7.0))*y+(4.7)-(-3.4)-(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((p+(-q)+(-q)/p)+p*(-q))/((q*(-3.3))-((-0.0)*(-p)/(-1.0)+p-8.3)*((-p))*(-q)+((-4.1)+0.4*(-2.8)))/((p/(-p)+(-p)*0.4*(-p))-q*(-p))-((q+(-7.0))*q+(4.7)-(-3.4)-(-q)))
            z2 <== (((x+(-y)+(-y)/x)+x*(-y))/((y*(-3.3))-((-0.0)*(-x)/(-1.0)+x-8.3)*((-x))*(-y)+((-4.1)+0.4*(-2.8)))/((x/(-x)+(-x)*0.4*(-x))-y*(-x))-((y+(-7.0))*y+(4.7)-(-3.4)-(-y)))
            wr.tt <| (I 269)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 270
        ctx.print.s "test270"
        //equation: ((-x)/(0.3+0.6+x-x)-8.2/x+y)
        let s = (((-x)/(0.3+0.6+x-x)-8.2/x+y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)/(0.3+0.6+p-p)-8.2/p+q)
            z2 <== ((-x)/(0.3+0.6+x-x)-8.2/x+y)
            wr.tt <| (I 270)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 271
        ctx.print.s "test271"
        //equation: ((y/((-4.1))-(y)+4.7*((-2.1))))
        let s = (((y/((-4.1))-(y)+4.7*((-2.1))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q/((-4.1))-(q)+4.7*((-2.1))))
            z2 <== ((y/((-4.1))-(y)+4.7*((-2.1))))
            wr.tt <| (I 271)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 272
        ctx.print.s "test272"
        //equation: ((((-3.8))-(0.8+2.5+(-y)-(-y)*0.0)*y+(-x)+(5.5+x-2.4/(-y)-8.8))/4.8/((-x)/8.5-y)+(-7.6))
        let s = (((((-3.8))-(0.8+2.5+(-y)-(-y)*0.0)*y+(-x)+(5.5+x-2.4/(-y)-8.8))/4.8/((-x)/8.5-y)+(-7.6))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-3.8))-(0.8+2.5+(-q)-(-q)*0.0)*q+(-p)+(5.5+p-2.4/(-q)-8.8))/4.8/((-p)/8.5-q)+(-7.6))
            z2 <== ((((-3.8))-(0.8+2.5+(-y)-(-y)*0.0)*y+(-x)+(5.5+x-2.4/(-y)-8.8))/4.8/((-x)/8.5-y)+(-7.6))
            wr.tt <| (I 272)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 273
        ctx.print.s "test273"
        //equation: (y-y/(-x)+((-x)*(-x)*(-0.5))+((-x))-1.7)
        let s = ((y-y/(-x)+((-x)*(-x)*(-0.5))+((-x))-1.7)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q-q/(-p)+((-p)*(-p)*(-0.5))+((-p))-1.7)
            z2 <== (y-y/(-x)+((-x)*(-x)*(-0.5))+((-x))-1.7)
            wr.tt <| (I 273)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 274
        ctx.print.s "test274"
        //equation: ((y/(-6.4)-(-0.0))*((-x)*(-3.2)*(-8.5)/y)-(-y)-(-x)*(y+3.2-x*(-4.6)/y)-(-2.2)/(-y)/4.0)
        let s = (((y/(-6.4)-(-0.0))*((-x)*(-3.2)*(-8.5)/y)-(-y)-(-x)*(y+3.2-x*(-4.6)/y)-(-2.2)/(-y)/4.0)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q/(-6.4)-(-0.0))*((-p)*(-3.2)*(-8.5)/q)-(-q)-(-p)*(q+3.2-p*(-4.6)/q)-(-2.2)/(-q)/4.0)
            z2 <== ((y/(-6.4)-(-0.0))*((-x)*(-3.2)*(-8.5)/y)-(-y)-(-x)*(y+3.2-x*(-4.6)/y)-(-2.2)/(-y)/4.0)
            wr.tt <| (I 274)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 275
        ctx.print.s "test275"
        //equation: ((-2.5)+(((-7.1))-(6.7-(-x)*(-6.1)+8.1-6.1)-5.2*y-(0.4+y+3.5+0.6+5.5))-(-2.8)+(x)+((-x)/((-2.6)/x-(-0.3)*0.4)+((-5.3)/(-x)-(-0.0)/(-0.1))+y-x))
        let s = (((-2.5)+(((-7.1))-(6.7-(-x)*(-6.1)+8.1-6.1)-5.2*y-(0.4+y+3.5+0.6+5.5))-(-2.8)+(x)+((-x)/((-2.6)/x-(-0.3)*0.4)+((-5.3)/(-x)-(-0.0)/(-0.1))+y-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-2.5)+(((-7.1))-(6.7-(-p)*(-6.1)+8.1-6.1)-5.2*q-(0.4+q+3.5+0.6+5.5))-(-2.8)+(p)+((-p)/((-2.6)/p-(-0.3)*0.4)+((-5.3)/(-p)-(-0.0)/(-0.1))+q-p))
            z2 <== ((-2.5)+(((-7.1))-(6.7-(-x)*(-6.1)+8.1-6.1)-5.2*y-(0.4+y+3.5+0.6+5.5))-(-2.8)+(x)+((-x)/((-2.6)/x-(-0.3)*0.4)+((-5.3)/(-x)-(-0.0)/(-0.1))+y-x))
            wr.tt <| (I 275)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 276
        ctx.print.s "test276"
        //equation: (-5.8)
        //printfn "%d" 277
        ctx.print.s "test277"
        //equation: ((-y)-(8.2-(7.8)))
        let s = (((-y)-(8.2-(7.8)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)-(8.2-(7.8)))
            z2 <== ((-y)-(8.2-(7.8)))
            wr.tt <| (I 277)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 278
        ctx.print.s "test278"
        //equation: x+x-(-x)
        let s = (x+x-(-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p+p-(-p)
            z2 <== x+x-(-x)
            wr.tt <| (I 278)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 279
        ctx.print.s "test279"
        //equation: 5.4
        //printfn "%d" 280
        ctx.print.s "test280"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 280)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 281
        ctx.print.s "test281"
        //equation: (-4.8)
        //printfn "%d" 282
        ctx.print.s "test282"
        //equation: ((0.5/(-x)-(-1.5)/(-8.6))-1.3+(-7.8)-y+(-7.4)+3.5*y*(-8.4)-8.6)
        let s = (((0.5/(-x)-(-1.5)/(-8.6))-1.3+(-7.8)-y+(-7.4)+3.5*y*(-8.4)-8.6)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((0.5/(-p)-(-1.5)/(-8.6))-1.3+(-7.8)-q+(-7.4)+3.5*q*(-8.4)-8.6)
            z2 <== ((0.5/(-x)-(-1.5)/(-8.6))-1.3+(-7.8)-y+(-7.4)+3.5*y*(-8.4)-8.6)
            wr.tt <| (I 282)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 283
        ctx.print.s "test283"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 283)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 284
        ctx.print.s "test284"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 284)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 285
        ctx.print.s "test285"
        //equation: (y+((-8.0)/(-3.0))*y/(x/((-4.2)))+(-1.4))
        let s = ((y+((-8.0)/(-3.0))*y/(x/((-4.2)))+(-1.4))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q+((-8.0)/(-3.0))*q/(p/((-4.2)))+(-1.4))
            z2 <== (y+((-8.0)/(-3.0))*y/(x/((-4.2)))+(-1.4))
            wr.tt <| (I 285)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 286
        ctx.print.s "test286"
        //equation: ((-y))
        let s = (((-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q))
            z2 <== ((-y))
            wr.tt <| (I 286)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 287
        ctx.print.s "test287"
        //equation: 0.5
        //printfn "%d" 288
        ctx.print.s "test288"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 288)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 289
        ctx.print.s "test289"
        //equation: (-1.6)
        //printfn "%d" 290
        ctx.print.s "test290"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 290)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 291
        ctx.print.s "test291"
        //equation: ((x-(-4.6)+(y/0.4-1.3*6.0)/x/(-0.3))+y/(-x)/(-x)*(-x)-x)
        let s = (((x-(-4.6)+(y/0.4-1.3*6.0)/x/(-0.3))+y/(-x)/(-x)*(-x)-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p-(-4.6)+(q/0.4-1.3*6.0)/p/(-0.3))+q/(-p)/(-p)*(-p)-p)
            z2 <== ((x-(-4.6)+(y/0.4-1.3*6.0)/x/(-0.3))+y/(-x)/(-x)*(-x)-x)
            wr.tt <| (I 291)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 292
        ctx.print.s "test292"
        //equation: (((-x)+((-y)/y)))
        let s = ((((-x)+((-y)/y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-p)+((-q)/q)))
            z2 <== (((-x)+((-y)/y)))
            wr.tt <| (I 292)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 293
        ctx.print.s "test293"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 293)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 294
        ctx.print.s "test294"
        //equation: (-3.7)
        //printfn "%d" 295
        ctx.print.s "test295"
        //equation: ((1.6)*((-x)-(-2.0)-5.8*y))*((-x)+(-8.7))/(1.4+(-2.4)/(-5.0)/x)*x+y+(-y)-(-8.3)*(-x)/1.8+(-y)+x-(7.4/(y)-((-y)*(-y)*(-0.2)))
        let s = (((1.6)*((-x)-(-2.0)-5.8*y))*((-x)+(-8.7))/(1.4+(-2.4)/(-5.0)/x)*x+y+(-y)-(-8.3)*(-x)/1.8+(-y)+x-(7.4/(y)-((-y)*(-y)*(-0.2)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((1.6)*((-p)-(-2.0)-5.8*q))*((-p)+(-8.7))/(1.4+(-2.4)/(-5.0)/p)*p+q+(-q)-(-8.3)*(-p)/1.8+(-q)+p-(7.4/(q)-((-q)*(-q)*(-0.2)))
            z2 <== ((1.6)*((-x)-(-2.0)-5.8*y))*((-x)+(-8.7))/(1.4+(-2.4)/(-5.0)/x)*x+y+(-y)-(-8.3)*(-x)/1.8+(-y)+x-(7.4/(y)-((-y)*(-y)*(-0.2)))
            wr.tt <| (I 295)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 296
        ctx.print.s "test296"
        //equation: (-2.2)
        //printfn "%d" 297
        ctx.print.s "test297"
        //equation: ((6.0+((-y))/(4.4*4.4)*y)/((-y)+((-x)-(-5.5)+2.7*(-1.5))-((-y)+(-x)-(-x)))-(-y)-(y*7.6-(-7.3)))
        let s = (((6.0+((-y))/(4.4*4.4)*y)/((-y)+((-x)-(-5.5)+2.7*(-1.5))-((-y)+(-x)-(-x)))-(-y)-(y*7.6-(-7.3)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((6.0+((-q))/(4.4*4.4)*q)/((-q)+((-p)-(-5.5)+2.7*(-1.5))-((-q)+(-p)-(-p)))-(-q)-(q*7.6-(-7.3)))
            z2 <== ((6.0+((-y))/(4.4*4.4)*y)/((-y)+((-x)-(-5.5)+2.7*(-1.5))-((-y)+(-x)-(-x)))-(-y)-(y*7.6-(-7.3)))
            wr.tt <| (I 297)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 298
        ctx.print.s "test298"
        //equation: (-1.1)
        //printfn "%d" 299
        ctx.print.s "test299"
        //equation: 8.6
        //printfn "%d" 300
        ctx.print.s "test300"
        //equation: (-7.2)
        //printfn "%d" 301
        ctx.print.s "test301"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 301)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 302
        ctx.print.s "test302"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 302)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 303
        ctx.print.s "test303"
        //equation: ((-0.7)+(-x))
        let s = (((-0.7)+(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-0.7)+(-p))
            z2 <== ((-0.7)+(-x))
            wr.tt <| (I 303)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 304
        ctx.print.s "test304"
        //equation: ((5.5/y-(-8.3))*((-y)+(x/7.5/(-6.0)*y*x)+6.8))
        let s = (((5.5/y-(-8.3))*((-y)+(x/7.5/(-6.0)*y*x)+6.8))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((5.5/q-(-8.3))*((-q)+(p/7.5/(-6.0)*q*p)+6.8))
            z2 <== ((5.5/y-(-8.3))*((-y)+(x/7.5/(-6.0)*y*x)+6.8))
            wr.tt <| (I 304)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 305
        ctx.print.s "test305"
        //equation: ((((-y))-(y-x/(-4.7))-(x+5.8)-y+(5.6-6.3-(-x)*5.0))/(-0.3)/(x)/((8.6/y+x)/y))
        let s = (((((-y))-(y-x/(-4.7))-(x+5.8)-y+(5.6-6.3-(-x)*5.0))/(-0.3)/(x)/((8.6/y+x)/y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-q))-(q-p/(-4.7))-(p+5.8)-q+(5.6-6.3-(-p)*5.0))/(-0.3)/(p)/((8.6/q+p)/q))
            z2 <== ((((-y))-(y-x/(-4.7))-(x+5.8)-y+(5.6-6.3-(-x)*5.0))/(-0.3)/(x)/((8.6/y+x)/y))
            wr.tt <| (I 305)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 306
        ctx.print.s "test306"
        //equation: 5.6
        //printfn "%d" 307
        ctx.print.s "test307"
        //equation: 2.4
        //printfn "%d" 308
        ctx.print.s "test308"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 308)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 309
        ctx.print.s "test309"
        //equation: (8.4+8.8+2.2/x+(-8.7)+(-8.2))
        let s = ((8.4+8.8+2.2/x+(-8.7)+(-8.2))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (8.4+8.8+2.2/p+(-8.7)+(-8.2))
            z2 <== (8.4+8.8+2.2/x+(-8.7)+(-8.2))
            wr.tt <| (I 309)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 310
        ctx.print.s "test310"
        //equation: ((y-(-x)-x/((-5.0)-(-x)+(-x)/(-7.4))-(3.4-(-y)/y/3.2/(-3.1))+x)*y+(-6.7)-4.5+y-((-y)-x-((-0.0)*(-0.4)*(-x))+2.3/8.1)/(((-1.3))*(-6.5)*(x+(-2.0)/(-y)+(-y)+(-6.8))/(x)))
        let s = (((y-(-x)-x/((-5.0)-(-x)+(-x)/(-7.4))-(3.4-(-y)/y/3.2/(-3.1))+x)*y+(-6.7)-4.5+y-((-y)-x-((-0.0)*(-0.4)*(-x))+2.3/8.1)/(((-1.3))*(-6.5)*(x+(-2.0)/(-y)+(-y)+(-6.8))/(x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q-(-p)-p/((-5.0)-(-p)+(-p)/(-7.4))-(3.4-(-q)/q/3.2/(-3.1))+p)*q+(-6.7)-4.5+q-((-q)-p-((-0.0)*(-0.4)*(-p))+2.3/8.1)/(((-1.3))*(-6.5)*(p+(-2.0)/(-q)+(-q)+(-6.8))/(p)))
            z2 <== ((y-(-x)-x/((-5.0)-(-x)+(-x)/(-7.4))-(3.4-(-y)/y/3.2/(-3.1))+x)*y+(-6.7)-4.5+y-((-y)-x-((-0.0)*(-0.4)*(-x))+2.3/8.1)/(((-1.3))*(-6.5)*(x+(-2.0)/(-y)+(-y)+(-6.8))/(x)))
            wr.tt <| (I 310)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 311
        ctx.print.s "test311"
        //equation: (-3.4)
        //printfn "%d" 312
        ctx.print.s "test312"
        //equation: (-1.0)
        //printfn "%d" 313
        ctx.print.s "test313"
        //equation: ((-4.4)*(-y)*x/(y+(-y)/x+(x*(-6.6)-(-x)*0.3*x)))
        let s = (((-4.4)*(-y)*x/(y+(-y)/x+(x*(-6.6)-(-x)*0.3*x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-4.4)*(-q)*p/(q+(-q)/p+(p*(-6.6)-(-p)*0.3*p)))
            z2 <== ((-4.4)*(-y)*x/(y+(-y)/x+(x*(-6.6)-(-x)*0.3*x)))
            wr.tt <| (I 313)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 314
        ctx.print.s "test314"
        //equation: (((-y)-(-x)-((-x)/y+(-x)*(-x))-x+2.4))
        let s = ((((-y)-(-x)-((-x)/y+(-x)*(-x))-x+2.4))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q)-(-p)-((-p)/q+(-p)*(-p))-p+2.4))
            z2 <== (((-y)-(-x)-((-x)/y+(-x)*(-x))-x+2.4))
            wr.tt <| (I 314)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 315
        ctx.print.s "test315"
        //equation: (-6.5)
        //printfn "%d" 316
        ctx.print.s "test316"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 316)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 317
        ctx.print.s "test317"
        //equation: (-0.1)
        //printfn "%d" 318
        ctx.print.s "test318"
        //equation: (-4.8)
        //printfn "%d" 319
        ctx.print.s "test319"
        //equation: ((-x)+((1.8/(-5.3)))/((0.3/5.4+x*y/5.4)))
        let s = (((-x)+((1.8/(-5.3)))/((0.3/5.4+x*y/5.4)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)+((1.8/(-5.3)))/((0.3/5.4+p*q/5.4)))
            z2 <== ((-x)+((1.8/(-5.3)))/((0.3/5.4+x*y/5.4)))
            wr.tt <| (I 319)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 320
        ctx.print.s "test320"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 320)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 321
        ctx.print.s "test321"
        //equation: ((-x)/4.7+(y)*(-4.2))
        let s = (((-x)/4.7+(y)*(-4.2))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)/4.7+(q)*(-4.2))
            z2 <== ((-x)/4.7+(y)*(-4.2))
            wr.tt <| (I 321)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 322
        ctx.print.s "test322"
        //equation: ((-x)+3.8/(-5.6)/(-0.3)-y*(-y)*((-y)-(-x)*5.3*((-1.0)-x/(-4.4)/(-6.7)-(-x))*(-2.0)+(-7.6)+(-y)))
        let s = (((-x)+3.8/(-5.6)/(-0.3)-y*(-y)*((-y)-(-x)*5.3*((-1.0)-x/(-4.4)/(-6.7)-(-x))*(-2.0)+(-7.6)+(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)+3.8/(-5.6)/(-0.3)-q*(-q)*((-q)-(-p)*5.3*((-1.0)-p/(-4.4)/(-6.7)-(-p))*(-2.0)+(-7.6)+(-q)))
            z2 <== ((-x)+3.8/(-5.6)/(-0.3)-y*(-y)*((-y)-(-x)*5.3*((-1.0)-x/(-4.4)/(-6.7)-(-x))*(-2.0)+(-7.6)+(-y)))
            wr.tt <| (I 322)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 323
        ctx.print.s "test323"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 323)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 324
        ctx.print.s "test324"
        //equation: ((-y))
        let s = (((-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q))
            z2 <== ((-y))
            wr.tt <| (I 324)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 325
        ctx.print.s "test325"
        //equation: (((-y)+y/(y+5.4+6.1-(-4.4)*3.4))/(-y)*x-(y+(-x)/((-0.4)+y*(-8.0)-1.1)+((-y)/x-(-8.2)-1.7)))
        let s = ((((-y)+y/(y+5.4+6.1-(-4.4)*3.4))/(-y)*x-(y+(-x)/((-0.4)+y*(-8.0)-1.1)+((-y)/x-(-8.2)-1.7)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q)+q/(q+5.4+6.1-(-4.4)*3.4))/(-q)*p-(q+(-p)/((-0.4)+q*(-8.0)-1.1)+((-q)/p-(-8.2)-1.7)))
            z2 <== (((-y)+y/(y+5.4+6.1-(-4.4)*3.4))/(-y)*x-(y+(-x)/((-0.4)+y*(-8.0)-1.1)+((-y)/x-(-8.2)-1.7)))
            wr.tt <| (I 325)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 326
        ctx.print.s "test326"
        //equation: ((-x)*((-y)/(-3.7)+(-7.6)*(-1.2))*y/(-1.3)*(((-x))*((-8.8)/y*(-5.4)+6.5)-3.5/y)*((-3.8)-(x+2.5)*((-y)+(-8.0)+(-3.4)-(-8.7))))
        let s = (((-x)*((-y)/(-3.7)+(-7.6)*(-1.2))*y/(-1.3)*(((-x))*((-8.8)/y*(-5.4)+6.5)-3.5/y)*((-3.8)-(x+2.5)*((-y)+(-8.0)+(-3.4)-(-8.7))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)*((-q)/(-3.7)+(-7.6)*(-1.2))*q/(-1.3)*(((-p))*((-8.8)/q*(-5.4)+6.5)-3.5/q)*((-3.8)-(p+2.5)*((-q)+(-8.0)+(-3.4)-(-8.7))))
            z2 <== ((-x)*((-y)/(-3.7)+(-7.6)*(-1.2))*y/(-1.3)*(((-x))*((-8.8)/y*(-5.4)+6.5)-3.5/y)*((-3.8)-(x+2.5)*((-y)+(-8.0)+(-3.4)-(-8.7))))
            wr.tt <| (I 326)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 327
        ctx.print.s "test327"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 327)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 328
        ctx.print.s "test328"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 328)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 329
        ctx.print.s "test329"
        //equation: ((-5.6)-((8.6*4.6/(-y))-x*(-6.8)+x)-(-8.8)+(((-x)+(-y)/(-y))*(x)+(-x)/y*((-y))))
        let s = (((-5.6)-((8.6*4.6/(-y))-x*(-6.8)+x)-(-8.8)+(((-x)+(-y)/(-y))*(x)+(-x)/y*((-y))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-5.6)-((8.6*4.6/(-q))-p*(-6.8)+p)-(-8.8)+(((-p)+(-q)/(-q))*(p)+(-p)/q*((-q))))
            z2 <== ((-5.6)-((8.6*4.6/(-y))-x*(-6.8)+x)-(-8.8)+(((-x)+(-y)/(-y))*(x)+(-x)/y*((-y))))
            wr.tt <| (I 329)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 330
        ctx.print.s "test330"
        //equation: ((-5.5)*(-7.8)/3.2)-6.3/(x/8.8/y)/(-0.6)*x*(-x)+(-y)+(-1.1)*3.7/(-6.4)-1.8*((7.4)/(5.1/4.7+x)-(1.5*x))
        let s = (((-5.5)*(-7.8)/3.2)-6.3/(x/8.8/y)/(-0.6)*x*(-x)+(-y)+(-1.1)*3.7/(-6.4)-1.8*((7.4)/(5.1/4.7+x)-(1.5*x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-5.5)*(-7.8)/3.2)-6.3/(p/8.8/q)/(-0.6)*p*(-p)+(-q)+(-1.1)*3.7/(-6.4)-1.8*((7.4)/(5.1/4.7+p)-(1.5*p))
            z2 <== ((-5.5)*(-7.8)/3.2)-6.3/(x/8.8/y)/(-0.6)*x*(-x)+(-y)+(-1.1)*3.7/(-6.4)-1.8*((7.4)/(5.1/4.7+x)-(1.5*x))
            wr.tt <| (I 330)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 331
        ctx.print.s "test331"
        //equation: ((-y)*((-3.4)/(-x)/(-2.4)-(-x)-3.4/(-1.8)))
        let s = (((-y)*((-3.4)/(-x)/(-2.4)-(-x)-3.4/(-1.8)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)*((-3.4)/(-p)/(-2.4)-(-p)-3.4/(-1.8)))
            z2 <== ((-y)*((-3.4)/(-x)/(-2.4)-(-x)-3.4/(-1.8)))
            wr.tt <| (I 331)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 332
        ctx.print.s "test332"
        //equation: ((((-y)/(-0.1))+(-y)+3.6/0.6*y*(-x)/x)*((-1.8)-y-x+((-5.4)-x*(-1.4)/x))*(y/5.3/((-y)*(-y)+(-x)+0.4-(-x))+(-y))-((-4.5)/7.8/(-y)/3.0)/(-y)*((-0.4)/(-x)-y/(-8.7)*(-1.8))*(-x))
        let s = (((((-y)/(-0.1))+(-y)+3.6/0.6*y*(-x)/x)*((-1.8)-y-x+((-5.4)-x*(-1.4)/x))*(y/5.3/((-y)*(-y)+(-x)+0.4-(-x))+(-y))-((-4.5)/7.8/(-y)/3.0)/(-y)*((-0.4)/(-x)-y/(-8.7)*(-1.8))*(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-q)/(-0.1))+(-q)+3.6/0.6*q*(-p)/p)*((-1.8)-q-p+((-5.4)-p*(-1.4)/p))*(q/5.3/((-q)*(-q)+(-p)+0.4-(-p))+(-q))-((-4.5)/7.8/(-q)/3.0)/(-q)*((-0.4)/(-p)-q/(-8.7)*(-1.8))*(-p))
            z2 <== ((((-y)/(-0.1))+(-y)+3.6/0.6*y*(-x)/x)*((-1.8)-y-x+((-5.4)-x*(-1.4)/x))*(y/5.3/((-y)*(-y)+(-x)+0.4-(-x))+(-y))-((-4.5)/7.8/(-y)/3.0)/(-y)*((-0.4)/(-x)-y/(-8.7)*(-1.8))*(-x))
            wr.tt <| (I 332)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 333
        ctx.print.s "test333"
        //equation: 8.1
        //printfn "%d" 334
        ctx.print.s "test334"
        //equation: (7.3/(((-3.3)/x/(-6.2)/(-x))+6.4*(-y))*2.3)
        let s = ((7.3/(((-3.3)/x/(-6.2)/(-x))+6.4*(-y))*2.3)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (7.3/(((-3.3)/p/(-6.2)/(-p))+6.4*(-q))*2.3)
            z2 <== (7.3/(((-3.3)/x/(-6.2)/(-x))+6.4*(-y))*2.3)
            wr.tt <| (I 334)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 335
        ctx.print.s "test335"
        //equation: ((-x)-(2.5+y)*(-x)+(-y))
        let s = (((-x)-(2.5+y)*(-x)+(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)-(2.5+q)*(-p)+(-q))
            z2 <== ((-x)-(2.5+y)*(-x)+(-y))
            wr.tt <| (I 335)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 336
        ctx.print.s "test336"
        //equation: (x-(-5.7)*(-5.2)/(-1.3)+(-2.1)*y/(((-4.1)-x))+x+((-7.3)-y+((-2.7))+x*((-y))))
        let s = ((x-(-5.7)*(-5.2)/(-1.3)+(-2.1)*y/(((-4.1)-x))+x+((-7.3)-y+((-2.7))+x*((-y))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p-(-5.7)*(-5.2)/(-1.3)+(-2.1)*q/(((-4.1)-p))+p+((-7.3)-q+((-2.7))+p*((-q))))
            z2 <== (x-(-5.7)*(-5.2)/(-1.3)+(-2.1)*y/(((-4.1)-x))+x+((-7.3)-y+((-2.7))+x*((-y))))
            wr.tt <| (I 336)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 337
        ctx.print.s "test337"
        //equation: ((-4.3)-(-x)+(x-(-4.3)-4.8)+(0.8*7.5-x+3.7)*x-(-0.4)*(((-1.8)-1.7-(-3.4)-(-1.5))*(-y)*((-y)*y/(-y)-6.8/(-6.4))))
        let s = (((-4.3)-(-x)+(x-(-4.3)-4.8)+(0.8*7.5-x+3.7)*x-(-0.4)*(((-1.8)-1.7-(-3.4)-(-1.5))*(-y)*((-y)*y/(-y)-6.8/(-6.4))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-4.3)-(-p)+(p-(-4.3)-4.8)+(0.8*7.5-p+3.7)*p-(-0.4)*(((-1.8)-1.7-(-3.4)-(-1.5))*(-q)*((-q)*q/(-q)-6.8/(-6.4))))
            z2 <== ((-4.3)-(-x)+(x-(-4.3)-4.8)+(0.8*7.5-x+3.7)*x-(-0.4)*(((-1.8)-1.7-(-3.4)-(-1.5))*(-y)*((-y)*y/(-y)-6.8/(-6.4))))
            wr.tt <| (I 337)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 338
        ctx.print.s "test338"
        //equation: 2.7
        //printfn "%d" 339
        ctx.print.s "test339"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 339)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 340
        ctx.print.s "test340"
        //equation: ((-x)+y*y-((x-(-y)*8.4)/(-2.5)-(6.6/(-x)*x)*((-2.4)/(-7.5)+(-1.1)-(-1.2)*(-7.0))+(5.8-3.1*y-(-5.6)-0.0))/(-6.0))
        let s = (((-x)+y*y-((x-(-y)*8.4)/(-2.5)-(6.6/(-x)*x)*((-2.4)/(-7.5)+(-1.1)-(-1.2)*(-7.0))+(5.8-3.1*y-(-5.6)-0.0))/(-6.0))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)+q*q-((p-(-q)*8.4)/(-2.5)-(6.6/(-p)*p)*((-2.4)/(-7.5)+(-1.1)-(-1.2)*(-7.0))+(5.8-3.1*q-(-5.6)-0.0))/(-6.0))
            z2 <== ((-x)+y*y-((x-(-y)*8.4)/(-2.5)-(6.6/(-x)*x)*((-2.4)/(-7.5)+(-1.1)-(-1.2)*(-7.0))+(5.8-3.1*y-(-5.6)-0.0))/(-6.0))
            wr.tt <| (I 340)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 341
        ctx.print.s "test341"
        //equation: 7.5
        //printfn "%d" 342
        ctx.print.s "test342"
        //equation: ((((-y)*3.1-(-x)+(-x))*(-x)+y/(-4.4)-(-1.5)-7.6/(-0.1)-4.3*((-0.2)-y/3.3))*4.4/((-y)*(-5.1)*((-x))-(y*7.1+y*6.4*y)*6.7)/(-0.3))
        let s = (((((-y)*3.1-(-x)+(-x))*(-x)+y/(-4.4)-(-1.5)-7.6/(-0.1)-4.3*((-0.2)-y/3.3))*4.4/((-y)*(-5.1)*((-x))-(y*7.1+y*6.4*y)*6.7)/(-0.3))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-q)*3.1-(-p)+(-p))*(-p)+q/(-4.4)-(-1.5)-7.6/(-0.1)-4.3*((-0.2)-q/3.3))*4.4/((-q)*(-5.1)*((-p))-(q*7.1+q*6.4*q)*6.7)/(-0.3))
            z2 <== ((((-y)*3.1-(-x)+(-x))*(-x)+y/(-4.4)-(-1.5)-7.6/(-0.1)-4.3*((-0.2)-y/3.3))*4.4/((-y)*(-5.1)*((-x))-(y*7.1+y*6.4*y)*6.7)/(-0.3))
            wr.tt <| (I 342)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 343
        ctx.print.s "test343"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 343)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 344
        ctx.print.s "test344"
        //equation: 1.5
        //printfn "%d" 345
        ctx.print.s "test345"
        //equation: (8.7-(-x)+((0.3)-(3.5*(-y)/y/7.2+(-2.7))*(-6.7)-(-3.4))/((-6.6)+(-1.3)))
        let s = ((8.7-(-x)+((0.3)-(3.5*(-y)/y/7.2+(-2.7))*(-6.7)-(-3.4))/((-6.6)+(-1.3)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (8.7-(-p)+((0.3)-(3.5*(-q)/q/7.2+(-2.7))*(-6.7)-(-3.4))/((-6.6)+(-1.3)))
            z2 <== (8.7-(-x)+((0.3)-(3.5*(-y)/y/7.2+(-2.7))*(-6.7)-(-3.4))/((-6.6)+(-1.3)))
            wr.tt <| (I 345)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 346
        ctx.print.s "test346"
        //equation: ((-0.1))
        //printfn "%d" 347
        ctx.print.s "test347"
        //equation: ((-y)+1.0+(y/((-0.5)+(-6.3)-(-x)/(-y))/(-x))-(2.8))
        let s = (((-y)+1.0+(y/((-0.5)+(-6.3)-(-x)/(-y))/(-x))-(2.8))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)+1.0+(q/((-0.5)+(-6.3)-(-p)/(-q))/(-p))-(2.8))
            z2 <== ((-y)+1.0+(y/((-0.5)+(-6.3)-(-x)/(-y))/(-x))-(2.8))
            wr.tt <| (I 347)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 348
        ctx.print.s "test348"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 348)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 349
        ctx.print.s "test349"
        //equation: 2.2
        //printfn "%d" 350
        ctx.print.s "test350"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 350)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 351
        ctx.print.s "test351"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 351)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 352
        ctx.print.s "test352"
        //equation: (3.7/x*(-y)*(-6.6)-3.2/(-4.2)+y/y+(-7.1))/(((-3.8)+5.4))-5.1/(-7.2)-4.5
        let s = ((3.7/x*(-y)*(-6.6)-3.2/(-4.2)+y/y+(-7.1))/(((-3.8)+5.4))-5.1/(-7.2)-4.5).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (3.7/p*(-q)*(-6.6)-3.2/(-4.2)+q/q+(-7.1))/(((-3.8)+5.4))-5.1/(-7.2)-4.5
            z2 <== (3.7/x*(-y)*(-6.6)-3.2/(-4.2)+y/y+(-7.1))/(((-3.8)+5.4))-5.1/(-7.2)-4.5
            wr.tt <| (I 352)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 353
        ctx.print.s "test353"
        //equation: ((((-8.8)+3.3+(-x)/(-6.7)*8.6)*(x-y+(-x)+1.0/4.3)-((-5.4)*x/(-x)*x)+8.5)-(-2.3)/(-7.7)/(6.6-(-1.1))/3.6*6.2)
        let s = (((((-8.8)+3.3+(-x)/(-6.7)*8.6)*(x-y+(-x)+1.0/4.3)-((-5.4)*x/(-x)*x)+8.5)-(-2.3)/(-7.7)/(6.6-(-1.1))/3.6*6.2)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-8.8)+3.3+(-p)/(-6.7)*8.6)*(p-q+(-p)+1.0/4.3)-((-5.4)*p/(-p)*p)+8.5)-(-2.3)/(-7.7)/(6.6-(-1.1))/3.6*6.2)
            z2 <== ((((-8.8)+3.3+(-x)/(-6.7)*8.6)*(x-y+(-x)+1.0/4.3)-((-5.4)*x/(-x)*x)+8.5)-(-2.3)/(-7.7)/(6.6-(-1.1))/3.6*6.2)
            wr.tt <| (I 353)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 354
        ctx.print.s "test354"
        //equation: (((x/(-y)/(-2.8)*(-y)))/(-5.5)*((-x)*(-x)-6.8-(-y)/y)+((-y)/(y+7.3+4.2*(-x)+(-y)))-(-4.6))
        let s = ((((x/(-y)/(-2.8)*(-y)))/(-5.5)*((-x)*(-x)-6.8-(-y)/y)+((-y)/(y+7.3+4.2*(-x)+(-y)))-(-4.6))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((p/(-q)/(-2.8)*(-q)))/(-5.5)*((-p)*(-p)-6.8-(-q)/q)+((-q)/(q+7.3+4.2*(-p)+(-q)))-(-4.6))
            z2 <== (((x/(-y)/(-2.8)*(-y)))/(-5.5)*((-x)*(-x)-6.8-(-y)/y)+((-y)/(y+7.3+4.2*(-x)+(-y)))-(-4.6))
            wr.tt <| (I 354)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 355
        ctx.print.s "test355"
        //equation: 6.7
        //printfn "%d" 356
        ctx.print.s "test356"
        //equation: (-2.8)
        //printfn "%d" 357
        ctx.print.s "test357"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 357)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 358
        ctx.print.s "test358"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 358)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 359
        ctx.print.s "test359"
        //equation: ((-y)-((-x)/7.5)*(-y)/(-x))
        let s = (((-y)-((-x)/7.5)*(-y)/(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)-((-p)/7.5)*(-q)/(-p))
            z2 <== ((-y)-((-x)/7.5)*(-y)/(-x))
            wr.tt <| (I 359)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 360
        ctx.print.s "test360"
        //equation: ((-y)+3.7)
        let s = (((-y)+3.7)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)+3.7)
            z2 <== ((-y)+3.7)
            wr.tt <| (I 360)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 361
        ctx.print.s "test361"
        //equation: 3.4
        //printfn "%d" 362
        ctx.print.s "test362"
        //equation: ((-5.8)-((-y)+x/x))
        let s = (((-5.8)-((-y)+x/x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-5.8)-((-q)+p/p))
            z2 <== ((-5.8)-((-y)+x/x))
            wr.tt <| (I 362)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 363
        ctx.print.s "test363"
        //equation: ((-4.3)-1.8-(((-x)/(-4.7))+((-4.1)+x*y)*(-y)+4.1/4.8))
        let s = (((-4.3)-1.8-(((-x)/(-4.7))+((-4.1)+x*y)*(-y)+4.1/4.8))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-4.3)-1.8-(((-p)/(-4.7))+((-4.1)+p*q)*(-q)+4.1/4.8))
            z2 <== ((-4.3)-1.8-(((-x)/(-4.7))+((-4.1)+x*y)*(-y)+4.1/4.8))
            wr.tt <| (I 363)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 364
        ctx.print.s "test364"
        //equation: ((-x)+(((-0.8)+(-0.3)/1.7-(-8.2)/3.1)/y))
        let s = (((-x)+(((-0.8)+(-0.3)/1.7-(-8.2)/3.1)/y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)+(((-0.8)+(-0.3)/1.7-(-8.2)/3.1)/q))
            z2 <== ((-x)+(((-0.8)+(-0.3)/1.7-(-8.2)/3.1)/y))
            wr.tt <| (I 364)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 365
        ctx.print.s "test365"
        //equation: (((-y)/(8.8+(-7.3))/8.2*(-4.1)/(-y))/(-0.6)-y+x)
        let s = ((((-y)/(8.8+(-7.3))/8.2*(-4.1)/(-y))/(-0.6)-y+x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q)/(8.8+(-7.3))/8.2*(-4.1)/(-q))/(-0.6)-q+p)
            z2 <== (((-y)/(8.8+(-7.3))/8.2*(-4.1)/(-y))/(-0.6)-y+x)
            wr.tt <| (I 365)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 366
        ctx.print.s "test366"
        //equation: 3.5*x-(2.1/(-5.3)-(-7.4)+(-y)/x)*(6.4/(-0.6)+y/(-5.0)+(-5.1))*((-x)*4.3+x-(-3.0)-4.1*7.1)+x-8.7
        let s = (3.5*x-(2.1/(-5.3)-(-7.4)+(-y)/x)*(6.4/(-0.6)+y/(-5.0)+(-5.1))*((-x)*4.3+x-(-3.0)-4.1*7.1)+x-8.7).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== 3.5*p-(2.1/(-5.3)-(-7.4)+(-q)/p)*(6.4/(-0.6)+q/(-5.0)+(-5.1))*((-p)*4.3+p-(-3.0)-4.1*7.1)+p-8.7
            z2 <== 3.5*x-(2.1/(-5.3)-(-7.4)+(-y)/x)*(6.4/(-0.6)+y/(-5.0)+(-5.1))*((-x)*4.3+x-(-3.0)-4.1*7.1)+x-8.7
            wr.tt <| (I 366)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 367
        ctx.print.s "test367"
        //equation: (-4.2)
        //printfn "%d" 368
        ctx.print.s "test368"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 368)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 369
        ctx.print.s "test369"
        //equation: 6.5
        //printfn "%d" 370
        ctx.print.s "test370"
        //equation: (((-4.6)-((-y))-((-6.1)+x-x/3.0))-((6.1/4.2-(-x)/(-4.7)+(-x)))/x/(-x)/((-y)/(7.6*(-x)-x/(-x))*(8.5*5.3+(-8.5)-(-x))+((-1.6)+(-x))))
        let s = ((((-4.6)-((-y))-((-6.1)+x-x/3.0))-((6.1/4.2-(-x)/(-4.7)+(-x)))/x/(-x)/((-y)/(7.6*(-x)-x/(-x))*(8.5*5.3+(-8.5)-(-x))+((-1.6)+(-x))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-4.6)-((-q))-((-6.1)+p-p/3.0))-((6.1/4.2-(-p)/(-4.7)+(-p)))/p/(-p)/((-q)/(7.6*(-p)-p/(-p))*(8.5*5.3+(-8.5)-(-p))+((-1.6)+(-p))))
            z2 <== (((-4.6)-((-y))-((-6.1)+x-x/3.0))-((6.1/4.2-(-x)/(-4.7)+(-x)))/x/(-x)/((-y)/(7.6*(-x)-x/(-x))*(8.5*5.3+(-8.5)-(-x))+((-1.6)+(-x))))
            wr.tt <| (I 370)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 371
        ctx.print.s "test371"
        //equation: ((-x)-0.1)-(-5.7)/(-y)-4.1*3.7/(1.6+(-y)*(-y)-(-6.5))*((-y)/y/6.7*(-2.4)+y)-((-y)+(-5.6))/((-1.8))/0.5*(-x)+2.3*(-x)*((3.4*y/3.0))
        let s = (((-x)-0.1)-(-5.7)/(-y)-4.1*3.7/(1.6+(-y)*(-y)-(-6.5))*((-y)/y/6.7*(-2.4)+y)-((-y)+(-5.6))/((-1.8))/0.5*(-x)+2.3*(-x)*((3.4*y/3.0))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)-0.1)-(-5.7)/(-q)-4.1*3.7/(1.6+(-q)*(-q)-(-6.5))*((-q)/q/6.7*(-2.4)+q)-((-q)+(-5.6))/((-1.8))/0.5*(-p)+2.3*(-p)*((3.4*q/3.0))
            z2 <== ((-x)-0.1)-(-5.7)/(-y)-4.1*3.7/(1.6+(-y)*(-y)-(-6.5))*((-y)/y/6.7*(-2.4)+y)-((-y)+(-5.6))/((-1.8))/0.5*(-x)+2.3*(-x)*((3.4*y/3.0))
            wr.tt <| (I 371)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 372
        ctx.print.s "test372"
        //equation: ((((-x)+(-x)-(-4.2)-5.7)-((-x)+y/(-2.6)))*(x)*(y-(-2.1)+((-y)))*y*((y/(-8.8)+x+5.4)/x+(-2.7)/((-1.8)-(-y)*5.7+1.2)+7.2))
        let s = (((((-x)+(-x)-(-4.2)-5.7)-((-x)+y/(-2.6)))*(x)*(y-(-2.1)+((-y)))*y*((y/(-8.8)+x+5.4)/x+(-2.7)/((-1.8)-(-y)*5.7+1.2)+7.2))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-p)+(-p)-(-4.2)-5.7)-((-p)+q/(-2.6)))*(p)*(q-(-2.1)+((-q)))*q*((q/(-8.8)+p+5.4)/p+(-2.7)/((-1.8)-(-q)*5.7+1.2)+7.2))
            z2 <== ((((-x)+(-x)-(-4.2)-5.7)-((-x)+y/(-2.6)))*(x)*(y-(-2.1)+((-y)))*y*((y/(-8.8)+x+5.4)/x+(-2.7)/((-1.8)-(-y)*5.7+1.2)+7.2))
            wr.tt <| (I 372)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 373
        ctx.print.s "test373"
        //equation: 4.1+(-0.6)-((-6.4)*y)/(y+y/8.5)-((-y)-6.6*(-x)-y)/y*(x+y*(-1.2)-6.1+0.7*(-y))*1.8
        let s = (4.1+(-0.6)-((-6.4)*y)/(y+y/8.5)-((-y)-6.6*(-x)-y)/y*(x+y*(-1.2)-6.1+0.7*(-y))*1.8).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== 4.1+(-0.6)-((-6.4)*q)/(q+q/8.5)-((-q)-6.6*(-p)-q)/q*(p+q*(-1.2)-6.1+0.7*(-q))*1.8
            z2 <== 4.1+(-0.6)-((-6.4)*y)/(y+y/8.5)-((-y)-6.6*(-x)-y)/y*(x+y*(-1.2)-6.1+0.7*(-y))*1.8
            wr.tt <| (I 373)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 374
        ctx.print.s "test374"
        //equation: (((5.2)+x/x+7.3)-(6.2-(-x)-x)-x+(-y)-((-x)-4.5))
        let s = ((((5.2)+x/x+7.3)-(6.2-(-x)-x)-x+(-y)-((-x)-4.5))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((5.2)+p/p+7.3)-(6.2-(-p)-p)-p+(-q)-((-p)-4.5))
            z2 <== (((5.2)+x/x+7.3)-(6.2-(-x)-x)-x+(-y)-((-x)-4.5))
            wr.tt <| (I 374)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 375
        ctx.print.s "test375"
        //equation: (((-1.7))/5.0*((-y)+(-y)+1.1-(-y)*x-(-x)/x)*x)
        let s = ((((-1.7))/5.0*((-y)+(-y)+1.1-(-y)*x-(-x)/x)*x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-1.7))/5.0*((-q)+(-q)+1.1-(-q)*p-(-p)/p)*p)
            z2 <== (((-1.7))/5.0*((-y)+(-y)+1.1-(-y)*x-(-x)/x)*x)
            wr.tt <| (I 375)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 376
        ctx.print.s "test376"
        //equation: (((-7.0)+y+(-y)+(-x))*(-x))*(-0.5)*(((-y)*4.2-(-y))+((-y)/(-2.2)+(-x)))+((-3.7))+(-x)
        let s = ((((-7.0)+y+(-y)+(-x))*(-x))*(-0.5)*(((-y)*4.2-(-y))+((-y)/(-2.2)+(-x)))+((-3.7))+(-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-7.0)+q+(-q)+(-p))*(-p))*(-0.5)*(((-q)*4.2-(-q))+((-q)/(-2.2)+(-p)))+((-3.7))+(-p)
            z2 <== (((-7.0)+y+(-y)+(-x))*(-x))*(-0.5)*(((-y)*4.2-(-y))+((-y)/(-2.2)+(-x)))+((-3.7))+(-x)
            wr.tt <| (I 376)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 377
        ctx.print.s "test377"
        //equation: (((7.0-(-y)*y/6.1)/((-x)*(-8.1))+((-2.7)/(-2.5))-6.8/x-(-5.3)*(-y))+(1.3+(2.5+(-4.2)/4.2-x)*((-x)/y/(-y)+y/2.7)*(y))-(((-y)-(-y)-y/(-6.4)*4.3)/(-y)-(x+(-5.3)+(-3.0))-((-6.2)/4.5))*(1.4/x))
        let s = ((((7.0-(-y)*y/6.1)/((-x)*(-8.1))+((-2.7)/(-2.5))-6.8/x-(-5.3)*(-y))+(1.3+(2.5+(-4.2)/4.2-x)*((-x)/y/(-y)+y/2.7)*(y))-(((-y)-(-y)-y/(-6.4)*4.3)/(-y)-(x+(-5.3)+(-3.0))-((-6.2)/4.5))*(1.4/x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((7.0-(-q)*q/6.1)/((-p)*(-8.1))+((-2.7)/(-2.5))-6.8/p-(-5.3)*(-q))+(1.3+(2.5+(-4.2)/4.2-p)*((-p)/q/(-q)+q/2.7)*(q))-(((-q)-(-q)-q/(-6.4)*4.3)/(-q)-(p+(-5.3)+(-3.0))-((-6.2)/4.5))*(1.4/p))
            z2 <== (((7.0-(-y)*y/6.1)/((-x)*(-8.1))+((-2.7)/(-2.5))-6.8/x-(-5.3)*(-y))+(1.3+(2.5+(-4.2)/4.2-x)*((-x)/y/(-y)+y/2.7)*(y))-(((-y)-(-y)-y/(-6.4)*4.3)/(-y)-(x+(-5.3)+(-3.0))-((-6.2)/4.5))*(1.4/x))
            wr.tt <| (I 377)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 378
        ctx.print.s "test378"
        //equation: (-2.0)
        //printfn "%d" 379
        ctx.print.s "test379"
        //equation: 0.6
        //printfn "%d" 380
        ctx.print.s "test380"
        //equation: ((-y)/3.7/((-5.7)+1.1+x)*0.4/(5.8*(-x)/(-x)+1.5)-(x+2.3)/(-6.5)*y-((3.8+(-7.4)-5.3+y)-((-y)/5.2-5.5+2.2)-x/(7.4+(-4.2)-(-0.7)*6.6)))
        let s = (((-y)/3.7/((-5.7)+1.1+x)*0.4/(5.8*(-x)/(-x)+1.5)-(x+2.3)/(-6.5)*y-((3.8+(-7.4)-5.3+y)-((-y)/5.2-5.5+2.2)-x/(7.4+(-4.2)-(-0.7)*6.6)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)/3.7/((-5.7)+1.1+p)*0.4/(5.8*(-p)/(-p)+1.5)-(p+2.3)/(-6.5)*q-((3.8+(-7.4)-5.3+q)-((-q)/5.2-5.5+2.2)-p/(7.4+(-4.2)-(-0.7)*6.6)))
            z2 <== ((-y)/3.7/((-5.7)+1.1+x)*0.4/(5.8*(-x)/(-x)+1.5)-(x+2.3)/(-6.5)*y-((3.8+(-7.4)-5.3+y)-((-y)/5.2-5.5+2.2)-x/(7.4+(-4.2)-(-0.7)*6.6)))
            wr.tt <| (I 380)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 381
        ctx.print.s "test381"
        //equation: 1.2
        //printfn "%d" 382
        ctx.print.s "test382"
        //equation: 8.3
        //printfn "%d" 383
        ctx.print.s "test383"
        //equation: (-5.8)
        //printfn "%d" 384
        ctx.print.s "test384"
        //equation: 5.1
        //printfn "%d" 385
        ctx.print.s "test385"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 385)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 386
        ctx.print.s "test386"
        //equation: 8.5
        //printfn "%d" 387
        ctx.print.s "test387"
        //equation: (y-(2.0)*y)
        let s = ((y-(2.0)*y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q-(2.0)*q)
            z2 <== (y-(2.0)*y)
            wr.tt <| (I 387)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 388
        ctx.print.s "test388"
        //equation: (7.0*((-7.1)/4.5*((-y)))+0.5)
        let s = ((7.0*((-7.1)/4.5*((-y)))+0.5)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (7.0*((-7.1)/4.5*((-q)))+0.5)
            z2 <== (7.0*((-7.1)/4.5*((-y)))+0.5)
            wr.tt <| (I 388)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 389
        ctx.print.s "test389"
        //equation: ((((-1.7)-8.8)/(-6.0))/8.8-(((-x)-x)/(x*(-8.7)+y+4.2)/((-y)*(-y)*(-5.1))))
        let s = (((((-1.7)-8.8)/(-6.0))/8.8-(((-x)-x)/(x*(-8.7)+y+4.2)/((-y)*(-y)*(-5.1))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-1.7)-8.8)/(-6.0))/8.8-(((-p)-p)/(p*(-8.7)+q+4.2)/((-q)*(-q)*(-5.1))))
            z2 <== ((((-1.7)-8.8)/(-6.0))/8.8-(((-x)-x)/(x*(-8.7)+y+4.2)/((-y)*(-y)*(-5.1))))
            wr.tt <| (I 389)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 390
        ctx.print.s "test390"
        //equation: (-0.0)
        //printfn "%d" 391
        ctx.print.s "test391"
        //equation: (y/(((-y)+(-y))-((-x))-2.1-x/1.1/2.1)+(((-x)*(-3.4))*(-y))-x-((-6.8)+5.1-(-4.8)-(-x)*4.5)-(8.6+5.6*(-x))+(-y)-2.0)
        let s = ((y/(((-y)+(-y))-((-x))-2.1-x/1.1/2.1)+(((-x)*(-3.4))*(-y))-x-((-6.8)+5.1-(-4.8)-(-x)*4.5)-(8.6+5.6*(-x))+(-y)-2.0)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q/(((-q)+(-q))-((-p))-2.1-p/1.1/2.1)+(((-p)*(-3.4))*(-q))-p-((-6.8)+5.1-(-4.8)-(-p)*4.5)-(8.6+5.6*(-p))+(-q)-2.0)
            z2 <== (y/(((-y)+(-y))-((-x))-2.1-x/1.1/2.1)+(((-x)*(-3.4))*(-y))-x-((-6.8)+5.1-(-4.8)-(-x)*4.5)-(8.6+5.6*(-x))+(-y)-2.0)
            wr.tt <| (I 391)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 392
        ctx.print.s "test392"
        //equation: 5.7
        //printfn "%d" 393
        ctx.print.s "test393"
        //equation: 1.5
        //printfn "%d" 394
        ctx.print.s "test394"
        //equation: 3.6
        //printfn "%d" 395
        ctx.print.s "test395"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 395)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 396
        ctx.print.s "test396"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 396)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 397
        ctx.print.s "test397"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 397)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 398
        ctx.print.s "test398"
        //equation: y*((x+y*3.0-(-x))*(-1.2)*((-2.1)*y-(-x)-0.2+(-x))*(-y)+(-x))-1.4
        let s = (y*((x+y*3.0-(-x))*(-1.2)*((-2.1)*y-(-x)-0.2+(-x))*(-y)+(-x))-1.4).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q*((p+q*3.0-(-p))*(-1.2)*((-2.1)*q-(-p)-0.2+(-p))*(-q)+(-p))-1.4
            z2 <== y*((x+y*3.0-(-x))*(-1.2)*((-2.1)*y-(-x)-0.2+(-x))*(-y)+(-x))-1.4
            wr.tt <| (I 398)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 399
        ctx.print.s "test399"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 399)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 400
        ctx.print.s "test400"
        //equation: 4.7
        //printfn "%d" 401
        ctx.print.s "test401"
        //equation: 0.1
        //printfn "%d" 402
        ctx.print.s "test402"
        //equation: (x-((3.1)/1.7)*(5.0/8.2-y*(7.3/6.1-0.3/(-7.0)))-((7.5-(-x))))
        let s = ((x-((3.1)/1.7)*(5.0/8.2-y*(7.3/6.1-0.3/(-7.0)))-((7.5-(-x))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p-((3.1)/1.7)*(5.0/8.2-q*(7.3/6.1-0.3/(-7.0)))-((7.5-(-p))))
            z2 <== (x-((3.1)/1.7)*(5.0/8.2-y*(7.3/6.1-0.3/(-7.0)))-((7.5-(-x))))
            wr.tt <| (I 402)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 403
        ctx.print.s "test403"
        //equation: (-7.0)
        //printfn "%d" 404
        ctx.print.s "test404"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 404)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 405
        ctx.print.s "test405"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 405)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 406
        ctx.print.s "test406"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 406)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 407
        ctx.print.s "test407"
        //equation: (0.5/y+((-8.6)+x/y-(-x)+x-x/(-4.0)-(-y))/((-8.0)*3.4)*(-0.2)+((-y)*(-y)*1.2/(-4.6))-(x*y)/(-y))
        let s = ((0.5/y+((-8.6)+x/y-(-x)+x-x/(-4.0)-(-y))/((-8.0)*3.4)*(-0.2)+((-y)*(-y)*1.2/(-4.6))-(x*y)/(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (0.5/q+((-8.6)+p/q-(-p)+p-p/(-4.0)-(-q))/((-8.0)*3.4)*(-0.2)+((-q)*(-q)*1.2/(-4.6))-(p*q)/(-q))
            z2 <== (0.5/y+((-8.6)+x/y-(-x)+x-x/(-4.0)-(-y))/((-8.0)*3.4)*(-0.2)+((-y)*(-y)*1.2/(-4.6))-(x*y)/(-y))
            wr.tt <| (I 407)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 408
        ctx.print.s "test408"
        //equation: (-4.3)
        //printfn "%d" 409
        ctx.print.s "test409"
        //equation: 1.1
        //printfn "%d" 410
        ctx.print.s "test410"
        //equation: (-x)+(-3.7)
        let s = ((-x)+(-3.7)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)+(-3.7)
            z2 <== (-x)+(-3.7)
            wr.tt <| (I 410)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 411
        ctx.print.s "test411"
        //equation: (x+6.8)
        let s = ((x+6.8)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p+6.8)
            z2 <== (x+6.8)
            wr.tt <| (I 411)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 412
        ctx.print.s "test412"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 412)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 413
        ctx.print.s "test413"
        //equation: (-5.6)
        //printfn "%d" 414
        ctx.print.s "test414"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 414)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 415
        ctx.print.s "test415"
        //equation: ((y/((-y))))
        let s = (((y/((-y))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q/((-q))))
            z2 <== ((y/((-y))))
            wr.tt <| (I 415)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 416
        ctx.print.s "test416"
        //equation: ((-6.1)/x*(6.3*(-x)-5.7/4.1)/(x*x/2.0)-((-y)/(-7.8)+(-2.5))+(y-((-7.4)-(-y)/(-x)/(-x)))*((-x)-(y/1.2*(-y)+(-y))/x-((-4.4)*x)/(-8.3)))
        let s = (((-6.1)/x*(6.3*(-x)-5.7/4.1)/(x*x/2.0)-((-y)/(-7.8)+(-2.5))+(y-((-7.4)-(-y)/(-x)/(-x)))*((-x)-(y/1.2*(-y)+(-y))/x-((-4.4)*x)/(-8.3)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-6.1)/p*(6.3*(-p)-5.7/4.1)/(p*p/2.0)-((-q)/(-7.8)+(-2.5))+(q-((-7.4)-(-q)/(-p)/(-p)))*((-p)-(q/1.2*(-q)+(-q))/p-((-4.4)*p)/(-8.3)))
            z2 <== ((-6.1)/x*(6.3*(-x)-5.7/4.1)/(x*x/2.0)-((-y)/(-7.8)+(-2.5))+(y-((-7.4)-(-y)/(-x)/(-x)))*((-x)-(y/1.2*(-y)+(-y))/x-((-4.4)*x)/(-8.3)))
            wr.tt <| (I 416)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 417
        ctx.print.s "test417"
        //equation: (-1.7)
        //printfn "%d" 418
        ctx.print.s "test418"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 418)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 419
        ctx.print.s "test419"
        //equation: ((-0.1)*4.2)
        //printfn "%d" 420
        ctx.print.s "test420"
        //equation: (((y-x*y)/2.3*y/(-y))*((y+7.6/(-8.3)*2.1+x)+(-7.8)*((-y)*(-y)+2.7)*y-5.8)*(-x)-3.1)
        let s = ((((y-x*y)/2.3*y/(-y))*((y+7.6/(-8.3)*2.1+x)+(-7.8)*((-y)*(-y)+2.7)*y-5.8)*(-x)-3.1)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((q-p*q)/2.3*q/(-q))*((q+7.6/(-8.3)*2.1+p)+(-7.8)*((-q)*(-q)+2.7)*q-5.8)*(-p)-3.1)
            z2 <== (((y-x*y)/2.3*y/(-y))*((y+7.6/(-8.3)*2.1+x)+(-7.8)*((-y)*(-y)+2.7)*y-5.8)*(-x)-3.1)
            wr.tt <| (I 420)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 421
        ctx.print.s "test421"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 421)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 422
        ctx.print.s "test422"
        //equation: (((-x)+(-5.5))*(-x)+(-7.7))
        let s = ((((-x)+(-5.5))*(-x)+(-7.7))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-p)+(-5.5))*(-p)+(-7.7))
            z2 <== (((-x)+(-5.5))*(-x)+(-7.7))
            wr.tt <| (I 422)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 423
        ctx.print.s "test423"
        //equation: (-8.2)
        //printfn "%d" 424
        ctx.print.s "test424"
        //equation: (y+((y-(-0.8))-y-x)*((-x)*4.5*y/(-x))/((6.8+3.4+(-0.1))+(-y))-((-8.6)/(-3.0)/(-x)-x))
        let s = ((y+((y-(-0.8))-y-x)*((-x)*4.5*y/(-x))/((6.8+3.4+(-0.1))+(-y))-((-8.6)/(-3.0)/(-x)-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q+((q-(-0.8))-q-p)*((-p)*4.5*q/(-p))/((6.8+3.4+(-0.1))+(-q))-((-8.6)/(-3.0)/(-p)-p))
            z2 <== (y+((y-(-0.8))-y-x)*((-x)*4.5*y/(-x))/((6.8+3.4+(-0.1))+(-y))-((-8.6)/(-3.0)/(-x)-x))
            wr.tt <| (I 424)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 425
        ctx.print.s "test425"
        //equation: 7.6
        //printfn "%d" 426
        ctx.print.s "test426"
        //equation: (((-x))+(6.0*(-x))+y/(-2.3)*(-y)-(-x)*(x+(-y)*y/(y)*x)*(-8.1))
        let s = ((((-x))+(6.0*(-x))+y/(-2.3)*(-y)-(-x)*(x+(-y)*y/(y)*x)*(-8.1))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-p))+(6.0*(-p))+q/(-2.3)*(-q)-(-p)*(p+(-q)*q/(q)*p)*(-8.1))
            z2 <== (((-x))+(6.0*(-x))+y/(-2.3)*(-y)-(-x)*(x+(-y)*y/(y)*x)*(-8.1))
            wr.tt <| (I 426)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 427
        ctx.print.s "test427"
        //equation: 7.7
        //printfn "%d" 428
        ctx.print.s "test428"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 428)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 429
        ctx.print.s "test429"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 429)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 430
        ctx.print.s "test430"
        //equation: (-6.4)
        //printfn "%d" 431
        ctx.print.s "test431"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 431)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 432
        ctx.print.s "test432"
        //equation: (-7.1)
        //printfn "%d" 433
        ctx.print.s "test433"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 433)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 434
        ctx.print.s "test434"
        //equation: ((-y)*7.7+(-y))
        let s = (((-y)*7.7+(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)*7.7+(-q))
            z2 <== ((-y)*7.7+(-y))
            wr.tt <| (I 434)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 435
        ctx.print.s "test435"
        //equation: ((-8.3))
        //printfn "%d" 436
        ctx.print.s "test436"
        //equation: (4.1/((-0.0)/y/(-y)*y-x)/(((-x))*x*(y-(-5.1)+(-x))*((-2.5)-(-7.7)*(-y)/x/(-8.5))-x))
        let s = ((4.1/((-0.0)/y/(-y)*y-x)/(((-x))*x*(y-(-5.1)+(-x))*((-2.5)-(-7.7)*(-y)/x/(-8.5))-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (4.1/((-0.0)/q/(-q)*q-p)/(((-p))*p*(q-(-5.1)+(-p))*((-2.5)-(-7.7)*(-q)/p/(-8.5))-p))
            z2 <== (4.1/((-0.0)/y/(-y)*y-x)/(((-x))*x*(y-(-5.1)+(-x))*((-2.5)-(-7.7)*(-y)/x/(-8.5))-x))
            wr.tt <| (I 436)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 437
        ctx.print.s "test437"
        //equation: (x/((-5.5))/0.8-y*y+(-x)*(-6.4)*((-4.2)+(-x))/y+y-(-y))
        let s = ((x/((-5.5))/0.8-y*y+(-x)*(-6.4)*((-4.2)+(-x))/y+y-(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p/((-5.5))/0.8-q*q+(-p)*(-6.4)*((-4.2)+(-p))/q+q-(-q))
            z2 <== (x/((-5.5))/0.8-y*y+(-x)*(-6.4)*((-4.2)+(-x))/y+y-(-y))
            wr.tt <| (I 437)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 438
        ctx.print.s "test438"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 438)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 439
        ctx.print.s "test439"
        //equation: (5.2/y+(((-7.0)+0.7/6.6))*((-6.5)))
        let s = ((5.2/y+(((-7.0)+0.7/6.6))*((-6.5)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (5.2/q+(((-7.0)+0.7/6.6))*((-6.5)))
            z2 <== (5.2/y+(((-7.0)+0.7/6.6))*((-6.5)))
            wr.tt <| (I 439)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 440
        ctx.print.s "test440"
        //equation: (-1.4)
        //printfn "%d" 441
        ctx.print.s "test441"
        //equation: ((-y)-4.2+(x*2.0*(-4.8)*y*y*(-8.8)+(-4.4)))
        let s = (((-y)-4.2+(x*2.0*(-4.8)*y*y*(-8.8)+(-4.4)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)-4.2+(p*2.0*(-4.8)*q*q*(-8.8)+(-4.4)))
            z2 <== ((-y)-4.2+(x*2.0*(-4.8)*y*y*(-8.8)+(-4.4)))
            wr.tt <| (I 441)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 442
        ctx.print.s "test442"
        //equation: 5.8
        //printfn "%d" 443
        ctx.print.s "test443"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 443)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 444
        ctx.print.s "test444"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 444)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 445
        ctx.print.s "test445"
        //equation: (1.0/x-((-x)+(-y)*5.7-2.3+(-2.4))-3.6/(-y))
        let s = ((1.0/x-((-x)+(-y)*5.7-2.3+(-2.4))-3.6/(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (1.0/p-((-p)+(-q)*5.7-2.3+(-2.4))-3.6/(-q))
            z2 <== (1.0/x-((-x)+(-y)*5.7-2.3+(-2.4))-3.6/(-y))
            wr.tt <| (I 445)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 446
        ctx.print.s "test446"
        //equation: (((-7.6))*(0.6-x+x*x-3.6+7.5*((-4.8)/y+2.8)/x)/(-4.3)/y+(x/(-2.5)))
        let s = ((((-7.6))*(0.6-x+x*x-3.6+7.5*((-4.8)/y+2.8)/x)/(-4.3)/y+(x/(-2.5)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-7.6))*(0.6-p+p*p-3.6+7.5*((-4.8)/q+2.8)/p)/(-4.3)/q+(p/(-2.5)))
            z2 <== (((-7.6))*(0.6-x+x*x-3.6+7.5*((-4.8)/y+2.8)/x)/(-4.3)/y+(x/(-2.5)))
            wr.tt <| (I 446)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 447
        ctx.print.s "test447"
        //equation: ((-0.8)*(5.0))*((x/0.1+3.2)+(x/y)+((-1.4)+x)/x-(-1.6)*5.2-((-y)*(-7.3)+(-x)))
        let s = (((-0.8)*(5.0))*((x/0.1+3.2)+(x/y)+((-1.4)+x)/x-(-1.6)*5.2-((-y)*(-7.3)+(-x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-0.8)*(5.0))*((p/0.1+3.2)+(p/q)+((-1.4)+p)/p-(-1.6)*5.2-((-q)*(-7.3)+(-p)))
            z2 <== ((-0.8)*(5.0))*((x/0.1+3.2)+(x/y)+((-1.4)+x)/x-(-1.6)*5.2-((-y)*(-7.3)+(-x)))
            wr.tt <| (I 447)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 448
        ctx.print.s "test448"
        //equation: (3.7-(-7.5)+(y-(-8.0)+(-4.3)-(-y)+x/x/y/(-7.7)+x)-(-3.1)-(((-4.4))*5.2/(x)*(-8.2)))
        let s = ((3.7-(-7.5)+(y-(-8.0)+(-4.3)-(-y)+x/x/y/(-7.7)+x)-(-3.1)-(((-4.4))*5.2/(x)*(-8.2)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (3.7-(-7.5)+(q-(-8.0)+(-4.3)-(-q)+p/p/q/(-7.7)+p)-(-3.1)-(((-4.4))*5.2/(p)*(-8.2)))
            z2 <== (3.7-(-7.5)+(y-(-8.0)+(-4.3)-(-y)+x/x/y/(-7.7)+x)-(-3.1)-(((-4.4))*5.2/(x)*(-8.2)))
            wr.tt <| (I 448)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 449
        ctx.print.s "test449"
        //equation: ((-y))
        let s = (((-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q))
            z2 <== ((-y))
            wr.tt <| (I 449)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 450
        ctx.print.s "test450"
        //equation: 5.5
        //printfn "%d" 451
        ctx.print.s "test451"
        //equation: (6.3*(-3.0)-y*(-y)*(-y))
        let s = ((6.3*(-3.0)-y*(-y)*(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (6.3*(-3.0)-q*(-q)*(-q))
            z2 <== (6.3*(-3.0)-y*(-y)*(-y))
            wr.tt <| (I 451)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 452
        ctx.print.s "test452"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 452)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 453
        ctx.print.s "test453"
        //equation: ((-x)-(-6.3)-(-x)/6.2/(-0.3)+8.8+((2.6+(-y)-x-(-2.8))*((-0.7)/2.7/0.1)-(-7.6)+((-x)*1.6)))
        let s = (((-x)-(-6.3)-(-x)/6.2/(-0.3)+8.8+((2.6+(-y)-x-(-2.8))*((-0.7)/2.7/0.1)-(-7.6)+((-x)*1.6)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)-(-6.3)-(-p)/6.2/(-0.3)+8.8+((2.6+(-q)-p-(-2.8))*((-0.7)/2.7/0.1)-(-7.6)+((-p)*1.6)))
            z2 <== ((-x)-(-6.3)-(-x)/6.2/(-0.3)+8.8+((2.6+(-y)-x-(-2.8))*((-0.7)/2.7/0.1)-(-7.6)+((-x)*1.6)))
            wr.tt <| (I 453)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 454
        ctx.print.s "test454"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 454)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 455
        ctx.print.s "test455"
        //equation: (-4.7)*(-7.7)-(-8.2)
        //printfn "%d" 456
        ctx.print.s "test456"
        //equation: ((((-8.6)/y-(-5.1)-(-5.5)+(-7.5))+(-0.2)/((-2.7)*(-2.4)/1.8*0.4*(-5.4))*(y*y+y/x)-(-8.2))/((x*7.0+x)*((-x)+0.1*(-x)*0.1)))
        let s = (((((-8.6)/y-(-5.1)-(-5.5)+(-7.5))+(-0.2)/((-2.7)*(-2.4)/1.8*0.4*(-5.4))*(y*y+y/x)-(-8.2))/((x*7.0+x)*((-x)+0.1*(-x)*0.1)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-8.6)/q-(-5.1)-(-5.5)+(-7.5))+(-0.2)/((-2.7)*(-2.4)/1.8*0.4*(-5.4))*(q*q+q/p)-(-8.2))/((p*7.0+p)*((-p)+0.1*(-p)*0.1)))
            z2 <== ((((-8.6)/y-(-5.1)-(-5.5)+(-7.5))+(-0.2)/((-2.7)*(-2.4)/1.8*0.4*(-5.4))*(y*y+y/x)-(-8.2))/((x*7.0+x)*((-x)+0.1*(-x)*0.1)))
            wr.tt <| (I 456)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 457
        ctx.print.s "test457"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 457)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 458
        ctx.print.s "test458"
        //equation: (x-(-y)-((-3.2)+4.8/x)*x)
        let s = ((x-(-y)-((-3.2)+4.8/x)*x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p-(-q)-((-3.2)+4.8/p)*p)
            z2 <== (x-(-y)-((-3.2)+4.8/x)*x)
            wr.tt <| (I 458)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 459
        ctx.print.s "test459"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 459)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 460
        ctx.print.s "test460"
        //equation: (-7.5)
        //printfn "%d" 461
        ctx.print.s "test461"
        //equation: 0.4*((0.6*(-8.6)+0.6+(-x))/5.5)-(-0.7)
        let s = (0.4*((0.6*(-8.6)+0.6+(-x))/5.5)-(-0.7)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== 0.4*((0.6*(-8.6)+0.6+(-p))/5.5)-(-0.7)
            z2 <== 0.4*((0.6*(-8.6)+0.6+(-x))/5.5)-(-0.7)
            wr.tt <| (I 461)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 462
        ctx.print.s "test462"
        //equation: (4.3*x*((-y)*(-y)*(-x)))*(((-2.0)*(-7.8)+4.6-2.5/y))/(-y)
        let s = ((4.3*x*((-y)*(-y)*(-x)))*(((-2.0)*(-7.8)+4.6-2.5/y))/(-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (4.3*p*((-q)*(-q)*(-p)))*(((-2.0)*(-7.8)+4.6-2.5/q))/(-q)
            z2 <== (4.3*x*((-y)*(-y)*(-x)))*(((-2.0)*(-7.8)+4.6-2.5/y))/(-y)
            wr.tt <| (I 462)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 463
        ctx.print.s "test463"
        //equation: (8.5+7.1)
        //printfn "%d" 464
        ctx.print.s "test464"
        //equation: (((-7.5)+(-4.5)*(-0.1)*0.4)+((-0.5))/(x)-((-x))-y+(1.2-5.8+4.0-(-0.5)*(-0.6)/7.6/(x*(-x)/x-(-5.1)))-((7.8*2.2+7.7)-y-(-8.6)+7.4-(-2.8)))
        let s = ((((-7.5)+(-4.5)*(-0.1)*0.4)+((-0.5))/(x)-((-x))-y+(1.2-5.8+4.0-(-0.5)*(-0.6)/7.6/(x*(-x)/x-(-5.1)))-((7.8*2.2+7.7)-y-(-8.6)+7.4-(-2.8)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-7.5)+(-4.5)*(-0.1)*0.4)+((-0.5))/(p)-((-p))-q+(1.2-5.8+4.0-(-0.5)*(-0.6)/7.6/(p*(-p)/p-(-5.1)))-((7.8*2.2+7.7)-q-(-8.6)+7.4-(-2.8)))
            z2 <== (((-7.5)+(-4.5)*(-0.1)*0.4)+((-0.5))/(x)-((-x))-y+(1.2-5.8+4.0-(-0.5)*(-0.6)/7.6/(x*(-x)/x-(-5.1)))-((7.8*2.2+7.7)-y-(-8.6)+7.4-(-2.8)))
            wr.tt <| (I 464)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 465
        ctx.print.s "test465"
        //equation: 5.1
        //printfn "%d" 466
        ctx.print.s "test466"
        //equation: 6.3
        //printfn "%d" 467
        ctx.print.s "test467"
        //equation: (y*0.7+(-x)*((4.1/3.0*(-y)+(-x))/2.2/x-(-y)-(-x)/(-0.0)-((-x)*6.5*x)))
        let s = ((y*0.7+(-x)*((4.1/3.0*(-y)+(-x))/2.2/x-(-y)-(-x)/(-0.0)-((-x)*6.5*x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q*0.7+(-p)*((4.1/3.0*(-q)+(-p))/2.2/p-(-q)-(-p)/(-0.0)-((-p)*6.5*p)))
            z2 <== (y*0.7+(-x)*((4.1/3.0*(-y)+(-x))/2.2/x-(-y)-(-x)/(-0.0)-((-x)*6.5*x)))
            wr.tt <| (I 467)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 468
        ctx.print.s "test468"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 468)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 469
        ctx.print.s "test469"
        //equation: ((-x)+(-x))
        let s = (((-x)+(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)+(-p))
            z2 <== ((-x)+(-x))
            wr.tt <| (I 469)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 470
        ctx.print.s "test470"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 470)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 471
        ctx.print.s "test471"
        //equation: ((((-x)-(-y))+(x-(-6.2)*3.7)/(-y))*(x/(-y))/3.8/(((-8.3)+x-x)+((-6.4)*(-y)))+(-x))
        let s = (((((-x)-(-y))+(x-(-6.2)*3.7)/(-y))*(x/(-y))/3.8/(((-8.3)+x-x)+((-6.4)*(-y)))+(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-p)-(-q))+(p-(-6.2)*3.7)/(-q))*(p/(-q))/3.8/(((-8.3)+p-p)+((-6.4)*(-q)))+(-p))
            z2 <== ((((-x)-(-y))+(x-(-6.2)*3.7)/(-y))*(x/(-y))/3.8/(((-8.3)+x-x)+((-6.4)*(-y)))+(-x))
            wr.tt <| (I 471)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 472
        ctx.print.s "test472"
        //equation: 8.5
        //printfn "%d" 473
        ctx.print.s "test473"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 473)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 474
        ctx.print.s "test474"
        //equation: 2.1*8.7/(-4.4)/(8.5)-8.3
        //printfn "%d" 475
        ctx.print.s "test475"
        //equation: (x*y*8.4/((-0.0)/4.0*(-2.6)*x/(-0.0))/((-y)/(-8.2)-y*(-5.6)))
        let s = ((x*y*8.4/((-0.0)/4.0*(-2.6)*x/(-0.0))/((-y)/(-8.2)-y*(-5.6)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p*q*8.4/((-0.0)/4.0*(-2.6)*p/(-0.0))/((-q)/(-8.2)-q*(-5.6)))
            z2 <== (x*y*8.4/((-0.0)/4.0*(-2.6)*x/(-0.0))/((-y)/(-8.2)-y*(-5.6)))
            wr.tt <| (I 475)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 476
        ctx.print.s "test476"
        //equation: (-x)*((2.0*(-1.8)+4.4)/1.8)-(-x)
        let s = ((-x)*((2.0*(-1.8)+4.4)/1.8)-(-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)*((2.0*(-1.8)+4.4)/1.8)-(-p)
            z2 <== (-x)*((2.0*(-1.8)+4.4)/1.8)-(-x)
            wr.tt <| (I 476)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 477
        ctx.print.s "test477"
        //equation: ((-x)+(-8.3))
        let s = (((-x)+(-8.3))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)+(-8.3))
            z2 <== ((-x)+(-8.3))
            wr.tt <| (I 477)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 478
        ctx.print.s "test478"
        //equation: x+8.7/((-3.6))+((-y))+((-3.0)-2.5+x)-((-7.1)*8.1)/(-2.2)
        let s = (x+8.7/((-3.6))+((-y))+((-3.0)-2.5+x)-((-7.1)*8.1)/(-2.2)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p+8.7/((-3.6))+((-q))+((-3.0)-2.5+p)-((-7.1)*8.1)/(-2.2)
            z2 <== x+8.7/((-3.6))+((-y))+((-3.0)-2.5+x)-((-7.1)*8.1)/(-2.2)
            wr.tt <| (I 478)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 479
        ctx.print.s "test479"
        //equation: (-y)+(-7.3)*(-x)*(-1.2)-x/((0.8-6.3-8.7*(-y)+6.4)/(y*x)/5.3)+(-x)
        let s = ((-y)+(-7.3)*(-x)*(-1.2)-x/((0.8-6.3-8.7*(-y)+6.4)/(y*x)/5.3)+(-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)+(-7.3)*(-p)*(-1.2)-p/((0.8-6.3-8.7*(-q)+6.4)/(q*p)/5.3)+(-p)
            z2 <== (-y)+(-7.3)*(-x)*(-1.2)-x/((0.8-6.3-8.7*(-y)+6.4)/(y*x)/5.3)+(-x)
            wr.tt <| (I 479)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 480
        ctx.print.s "test480"
        //equation: (y*(1.7+y/(-x))*0.2/(-y))/7.7-8.3/(-8.5)-(-y)
        let s = ((y*(1.7+y/(-x))*0.2/(-y))/7.7-8.3/(-8.5)-(-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q*(1.7+q/(-p))*0.2/(-q))/7.7-8.3/(-8.5)-(-q)
            z2 <== (y*(1.7+y/(-x))*0.2/(-y))/7.7-8.3/(-8.5)-(-y)
            wr.tt <| (I 480)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 481
        ctx.print.s "test481"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 481)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 482
        ctx.print.s "test482"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 482)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 483
        ctx.print.s "test483"
        //equation: (x/(((-x)*x+0.4))+y+2.1-(5.0))
        let s = ((x/(((-x)*x+0.4))+y+2.1-(5.0))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p/(((-p)*p+0.4))+q+2.1-(5.0))
            z2 <== (x/(((-x)*x+0.4))+y+2.1-(5.0))
            wr.tt <| (I 483)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 484
        ctx.print.s "test484"
        //equation: (-y)/(y+(-6.8)+7.1*((-y)+(-6.6)-(-7.5)+(-x)*(-0.7))-(-1.6)-(-6.6))+(-x)
        let s = ((-y)/(y+(-6.8)+7.1*((-y)+(-6.6)-(-7.5)+(-x)*(-0.7))-(-1.6)-(-6.6))+(-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)/(q+(-6.8)+7.1*((-q)+(-6.6)-(-7.5)+(-p)*(-0.7))-(-1.6)-(-6.6))+(-p)
            z2 <== (-y)/(y+(-6.8)+7.1*((-y)+(-6.6)-(-7.5)+(-x)*(-0.7))-(-1.6)-(-6.6))+(-x)
            wr.tt <| (I 484)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 485
        ctx.print.s "test485"
        //equation: (5.0*x*(y*(-x))+(x-(-x))+(6.1)-y/(-x)*(-y)-(-x))
        let s = ((5.0*x*(y*(-x))+(x-(-x))+(6.1)-y/(-x)*(-y)-(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (5.0*p*(q*(-p))+(p-(-p))+(6.1)-q/(-p)*(-q)-(-p))
            z2 <== (5.0*x*(y*(-x))+(x-(-x))+(6.1)-y/(-x)*(-y)-(-x))
            wr.tt <| (I 485)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 486
        ctx.print.s "test486"
        //equation: (((x)/((-y)+(-y)*7.0)))
        let s = ((((x)/((-y)+(-y)*7.0)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((p)/((-q)+(-q)*7.0)))
            z2 <== (((x)/((-y)+(-y)*7.0)))
            wr.tt <| (I 486)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 487
        ctx.print.s "test487"
        //equation: 7.7
        //printfn "%d" 488
        ctx.print.s "test488"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 488)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 489
        ctx.print.s "test489"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 489)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 490
        ctx.print.s "test490"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 490)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 491
        ctx.print.s "test491"
        //equation: (-4.7)
        //printfn "%d" 492
        ctx.print.s "test492"
        //equation: (4.8)
        //printfn "%d" 493
        ctx.print.s "test493"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 493)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 494
        ctx.print.s "test494"
        //equation: (6.7)
        //printfn "%d" 495
        ctx.print.s "test495"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 495)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 496
        ctx.print.s "test496"
        //equation: (0.4-7.8/(-y)*((-8.2)+y+2.4/x)*x*8.4)
        let s = ((0.4-7.8/(-y)*((-8.2)+y+2.4/x)*x*8.4)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (0.4-7.8/(-q)*((-8.2)+q+2.4/p)*p*8.4)
            z2 <== (0.4-7.8/(-y)*((-8.2)+y+2.4/x)*x*8.4)
            wr.tt <| (I 496)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 497
        ctx.print.s "test497"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 497)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 498
        ctx.print.s "test498"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 498)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 499
        ctx.print.s "test499"
        //equation: (((-4.0)))
        //printfn "%d" 500
        ctx.print.s "test500"
        //equation: (y+(-8.7)/(y/0.3)*(-2.3)+(-y)-4.8)
        let s = ((y+(-8.7)/(y/0.3)*(-2.3)+(-y)-4.8)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q+(-8.7)/(q/0.3)*(-2.3)+(-q)-4.8)
            z2 <== (y+(-8.7)/(y/0.3)*(-2.3)+(-y)-4.8)
            wr.tt <| (I 500)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 501
        ctx.print.s "test501"
        //equation: ((-x)+(-y))
        let s = (((-x)+(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)+(-q))
            z2 <== ((-x)+(-y))
            wr.tt <| (I 501)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 502
        ctx.print.s "test502"
        //equation: 7.6
        //printfn "%d" 503
        ctx.print.s "test503"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 503)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 504
        ctx.print.s "test504"
        //equation: ((-1.6)-(x/(-4.4)*x/y+7.7)+y*(x+(-8.5)*(-y)*(-5.0)/(-x)))+((x)*(-y)-(0.3*6.2*(-7.3)+5.3)*(-x)+(-6.4)+x+(-x))-((-5.7)-3.5)
        let s = (((-1.6)-(x/(-4.4)*x/y+7.7)+y*(x+(-8.5)*(-y)*(-5.0)/(-x)))+((x)*(-y)-(0.3*6.2*(-7.3)+5.3)*(-x)+(-6.4)+x+(-x))-((-5.7)-3.5)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-1.6)-(p/(-4.4)*p/q+7.7)+q*(p+(-8.5)*(-q)*(-5.0)/(-p)))+((p)*(-q)-(0.3*6.2*(-7.3)+5.3)*(-p)+(-6.4)+p+(-p))-((-5.7)-3.5)
            z2 <== ((-1.6)-(x/(-4.4)*x/y+7.7)+y*(x+(-8.5)*(-y)*(-5.0)/(-x)))+((x)*(-y)-(0.3*6.2*(-7.3)+5.3)*(-x)+(-6.4)+x+(-x))-((-5.7)-3.5)
            wr.tt <| (I 504)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 505
        ctx.print.s "test505"
        //equation: (y+(x-x-(-x)-((-y)-y)*x-(-y)-(-y))/x)
        let s = ((y+(x-x-(-x)-((-y)-y)*x-(-y)-(-y))/x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q+(p-p-(-p)-((-q)-q)*p-(-q)-(-q))/p)
            z2 <== (y+(x-x-(-x)-((-y)-y)*x-(-y)-(-y))/x)
            wr.tt <| (I 505)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 506
        ctx.print.s "test506"
        //equation: ((-1.8)*((-y)*(-y)/(-5.7)*(5.7+(-y)+0.6/(-7.6)-(-8.5)))+(-0.1)*8.2+(-y)+x)
        let s = (((-1.8)*((-y)*(-y)/(-5.7)*(5.7+(-y)+0.6/(-7.6)-(-8.5)))+(-0.1)*8.2+(-y)+x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-1.8)*((-q)*(-q)/(-5.7)*(5.7+(-q)+0.6/(-7.6)-(-8.5)))+(-0.1)*8.2+(-q)+p)
            z2 <== ((-1.8)*((-y)*(-y)/(-5.7)*(5.7+(-y)+0.6/(-7.6)-(-8.5)))+(-0.1)*8.2+(-y)+x)
            wr.tt <| (I 506)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 507
        ctx.print.s "test507"
        //equation: (-8.8)
        //printfn "%d" 508
        ctx.print.s "test508"
        //equation: ((x/3.4/0.5)+(-y)/(-1.5)-4.2+(-x)-3.2/6.8/(x+3.5+(-y)+2.0/(-0.0))+x/y)
        let s = (((x/3.4/0.5)+(-y)/(-1.5)-4.2+(-x)-3.2/6.8/(x+3.5+(-y)+2.0/(-0.0))+x/y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p/3.4/0.5)+(-q)/(-1.5)-4.2+(-p)-3.2/6.8/(p+3.5+(-q)+2.0/(-0.0))+p/q)
            z2 <== ((x/3.4/0.5)+(-y)/(-1.5)-4.2+(-x)-3.2/6.8/(x+3.5+(-y)+2.0/(-0.0))+x/y)
            wr.tt <| (I 508)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 509
        ctx.print.s "test509"
        //equation: ((-0.4)-(6.0*(-y)/(-4.6)-(-y))+(x*(-6.7))+(1.3/(-5.6)+x)-(-y)-x)
        let s = (((-0.4)-(6.0*(-y)/(-4.6)-(-y))+(x*(-6.7))+(1.3/(-5.6)+x)-(-y)-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-0.4)-(6.0*(-q)/(-4.6)-(-q))+(p*(-6.7))+(1.3/(-5.6)+p)-(-q)-p)
            z2 <== ((-0.4)-(6.0*(-y)/(-4.6)-(-y))+(x*(-6.7))+(1.3/(-5.6)+x)-(-y)-x)
            wr.tt <| (I 509)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 510
        ctx.print.s "test510"
        //equation: ((x-((-x)+(-7.7))))
        let s = (((x-((-x)+(-7.7))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p-((-p)+(-7.7))))
            z2 <== ((x-((-x)+(-7.7))))
            wr.tt <| (I 510)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 511
        ctx.print.s "test511"
        //equation: ((-x)+x*(-4.7)/x/(-5.1)-(-y)-y-0.5*5.7)/((x/(-y))/5.3*(-6.0)*(-y)+(-y))-((3.1+y+2.3-x/4.5)/(x)+(-0.3)/5.4-(-x))
        let s = (((-x)+x*(-4.7)/x/(-5.1)-(-y)-y-0.5*5.7)/((x/(-y))/5.3*(-6.0)*(-y)+(-y))-((3.1+y+2.3-x/4.5)/(x)+(-0.3)/5.4-(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)+p*(-4.7)/p/(-5.1)-(-q)-q-0.5*5.7)/((p/(-q))/5.3*(-6.0)*(-q)+(-q))-((3.1+q+2.3-p/4.5)/(p)+(-0.3)/5.4-(-p))
            z2 <== ((-x)+x*(-4.7)/x/(-5.1)-(-y)-y-0.5*5.7)/((x/(-y))/5.3*(-6.0)*(-y)+(-y))-((3.1+y+2.3-x/4.5)/(x)+(-0.3)/5.4-(-x))
            wr.tt <| (I 511)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 512
        ctx.print.s "test512"
        //equation: (-8.3)
        //printfn "%d" 513
        ctx.print.s "test513"
        //equation: ((5.4*y+(-y)*x*(-7.3)*((-x)/x)+((-3.5)+(-x)-(-y)*(-7.7)/7.0))/x)
        let s = (((5.4*y+(-y)*x*(-7.3)*((-x)/x)+((-3.5)+(-x)-(-y)*(-7.7)/7.0))/x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((5.4*q+(-q)*p*(-7.3)*((-p)/p)+((-3.5)+(-p)-(-q)*(-7.7)/7.0))/p)
            z2 <== ((5.4*y+(-y)*x*(-7.3)*((-x)/x)+((-3.5)+(-x)-(-y)*(-7.7)/7.0))/x)
            wr.tt <| (I 513)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 514
        ctx.print.s "test514"
        //equation: (x+((5.1+(-1.1)+y*(-y))+(-1.5))/(-y)*((-8.8)*((-4.8)+y/7.4-x-(-2.5))/(-x)/y)/(-3.3))
        let s = ((x+((5.1+(-1.1)+y*(-y))+(-1.5))/(-y)*((-8.8)*((-4.8)+y/7.4-x-(-2.5))/(-x)/y)/(-3.3))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p+((5.1+(-1.1)+q*(-q))+(-1.5))/(-q)*((-8.8)*((-4.8)+q/7.4-p-(-2.5))/(-p)/q)/(-3.3))
            z2 <== (x+((5.1+(-1.1)+y*(-y))+(-1.5))/(-y)*((-8.8)*((-4.8)+y/7.4-x-(-2.5))/(-x)/y)/(-3.3))
            wr.tt <| (I 514)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 515
        ctx.print.s "test515"
        //equation: (((-5.6))+((-0.2)+y))
        let s = ((((-5.6))+((-0.2)+y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-5.6))+((-0.2)+q))
            z2 <== (((-5.6))+((-0.2)+y))
            wr.tt <| (I 515)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 516
        ctx.print.s "test516"
        //equation: 5.4/5.3*(y)+((-5.2)*(-7.3)*(-x))/x*(x)/(-x)*(3.7-(-y)+(-1.7)*1.8)-((-5.5)-7.3-y/1.7)*(-y)*(-2.7)*x*x/(-x)*(y/(y+y/7.7))
        let s = (5.4/5.3*(y)+((-5.2)*(-7.3)*(-x))/x*(x)/(-x)*(3.7-(-y)+(-1.7)*1.8)-((-5.5)-7.3-y/1.7)*(-y)*(-2.7)*x*x/(-x)*(y/(y+y/7.7))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== 5.4/5.3*(q)+((-5.2)*(-7.3)*(-p))/p*(p)/(-p)*(3.7-(-q)+(-1.7)*1.8)-((-5.5)-7.3-q/1.7)*(-q)*(-2.7)*p*p/(-p)*(q/(q+q/7.7))
            z2 <== 5.4/5.3*(y)+((-5.2)*(-7.3)*(-x))/x*(x)/(-x)*(3.7-(-y)+(-1.7)*1.8)-((-5.5)-7.3-y/1.7)*(-y)*(-2.7)*x*x/(-x)*(y/(y+y/7.7))
            wr.tt <| (I 516)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 517
        ctx.print.s "test517"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 517)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 518
        ctx.print.s "test518"
        //equation: 3.3
        //printfn "%d" 519
        ctx.print.s "test519"
        //equation: 0.4*(-x)
        let s = (0.4*(-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== 0.4*(-p)
            z2 <== 0.4*(-x)
            wr.tt <| (I 519)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 520
        ctx.print.s "test520"
        //equation: 2.8
        //printfn "%d" 521
        ctx.print.s "test521"
        //equation: 3.5
        //printfn "%d" 522
        ctx.print.s "test522"
        //equation: (8.2)
        //printfn "%d" 523
        ctx.print.s "test523"
        //equation: (((x-y)-(-2.0)*(y/(-1.0)))/y-(-x)*4.5-(((-x)-x+(-y))))
        let s = ((((x-y)-(-2.0)*(y/(-1.0)))/y-(-x)*4.5-(((-x)-x+(-y))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((p-q)-(-2.0)*(q/(-1.0)))/q-(-p)*4.5-(((-p)-p+(-q))))
            z2 <== (((x-y)-(-2.0)*(y/(-1.0)))/y-(-x)*4.5-(((-x)-x+(-y))))
            wr.tt <| (I 523)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 524
        ctx.print.s "test524"
        //equation: ((((-3.8))-x-(0.0/0.2+8.6+2.2)-x))
        let s = (((((-3.8))-x-(0.0/0.2+8.6+2.2)-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-3.8))-p-(0.0/0.2+8.6+2.2)-p))
            z2 <== ((((-3.8))-x-(0.0/0.2+8.6+2.2)-x))
            wr.tt <| (I 524)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 525
        ctx.print.s "test525"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 525)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 526
        ctx.print.s "test526"
        //equation: 2.1
        //printfn "%d" 527
        ctx.print.s "test527"
        //equation: 6.5
        //printfn "%d" 528
        ctx.print.s "test528"
        //equation: (((-x))*2.0+(-x)*((-5.2)-y/(-y)/y+(-6.0))/y+x*6.6*x)
        let s = ((((-x))*2.0+(-x)*((-5.2)-y/(-y)/y+(-6.0))/y+x*6.6*x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-p))*2.0+(-p)*((-5.2)-q/(-q)/q+(-6.0))/q+p*6.6*p)
            z2 <== (((-x))*2.0+(-x)*((-5.2)-y/(-y)/y+(-6.0))/y+x*6.6*x)
            wr.tt <| (I 528)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 529
        ctx.print.s "test529"
        //equation: ((x-(-y)-(-6.5)*4.2/8.5))
        let s = (((x-(-y)-(-6.5)*4.2/8.5))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p-(-q)-(-6.5)*4.2/8.5))
            z2 <== ((x-(-y)-(-6.5)*4.2/8.5))
            wr.tt <| (I 529)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 530
        ctx.print.s "test530"
        //equation: (-5.8)
        //printfn "%d" 531
        ctx.print.s "test531"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 531)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 532
        ctx.print.s "test532"
        //equation: (2.6)
        //printfn "%d" 533
        ctx.print.s "test533"
        //equation: (5.7)
        //printfn "%d" 534
        ctx.print.s "test534"
        //equation: (-y)-x*(4.8+x-y+(y))+2.8/8.2
        let s = ((-y)-x*(4.8+x-y+(y))+2.8/8.2).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)-p*(4.8+p-q+(q))+2.8/8.2
            z2 <== (-y)-x*(4.8+x-y+(y))+2.8/8.2
            wr.tt <| (I 534)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 535
        ctx.print.s "test535"
        //equation: (-y)/(((-y)*(-y)+1.3/6.4)-(-8.2)+((-x)+(-y)*4.5)*(-7.6))-(-7.4)*(5.5/(-x)*(-1.2)/(-y)+4.6)+y-(-y)-y*(-5.1)
        let s = ((-y)/(((-y)*(-y)+1.3/6.4)-(-8.2)+((-x)+(-y)*4.5)*(-7.6))-(-7.4)*(5.5/(-x)*(-1.2)/(-y)+4.6)+y-(-y)-y*(-5.1)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)/(((-q)*(-q)+1.3/6.4)-(-8.2)+((-p)+(-q)*4.5)*(-7.6))-(-7.4)*(5.5/(-p)*(-1.2)/(-q)+4.6)+q-(-q)-q*(-5.1)
            z2 <== (-y)/(((-y)*(-y)+1.3/6.4)-(-8.2)+((-x)+(-y)*4.5)*(-7.6))-(-7.4)*(5.5/(-x)*(-1.2)/(-y)+4.6)+y-(-y)-y*(-5.1)
            wr.tt <| (I 535)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 536
        ctx.print.s "test536"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 536)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 537
        ctx.print.s "test537"
        //equation: 4.7
        //printfn "%d" 538
        ctx.print.s "test538"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 538)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 539
        ctx.print.s "test539"
        //equation: (-8.0)
        //printfn "%d" 540
        ctx.print.s "test540"
        //equation: (-0.1)
        //printfn "%d" 541
        ctx.print.s "test541"
        //equation: ((4.0+(x*y+(-1.7)*0.5+5.8)+8.5)/(1.7-(-x)+y)+8.0)
        let s = (((4.0+(x*y+(-1.7)*0.5+5.8)+8.5)/(1.7-(-x)+y)+8.0)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((4.0+(p*q+(-1.7)*0.5+5.8)+8.5)/(1.7-(-p)+q)+8.0)
            z2 <== ((4.0+(x*y+(-1.7)*0.5+5.8)+8.5)/(1.7-(-x)+y)+8.0)
            wr.tt <| (I 541)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 542
        ctx.print.s "test542"
        //equation: (x)
        let s = ((x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p)
            z2 <== (x)
            wr.tt <| (I 542)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 543
        ctx.print.s "test543"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 543)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 544
        ctx.print.s "test544"
        //equation: 5.6
        //printfn "%d" 545
        ctx.print.s "test545"
        //equation: (-8.7)
        //printfn "%d" 546
        ctx.print.s "test546"
        //equation: ((-x)*(1.5*5.1/(-x)/(-y)))-6.7/6.1/((-x)+(-8.0)+(-y)*(-8.5)/x/(-3.8)/(-x)/6.7/x)
        let s = (((-x)*(1.5*5.1/(-x)/(-y)))-6.7/6.1/((-x)+(-8.0)+(-y)*(-8.5)/x/(-3.8)/(-x)/6.7/x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)*(1.5*5.1/(-p)/(-q)))-6.7/6.1/((-p)+(-8.0)+(-q)*(-8.5)/p/(-3.8)/(-p)/6.7/p)
            z2 <== ((-x)*(1.5*5.1/(-x)/(-y)))-6.7/6.1/((-x)+(-8.0)+(-y)*(-8.5)/x/(-3.8)/(-x)/6.7/x)
            wr.tt <| (I 546)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 547
        ctx.print.s "test547"
        //equation: ((-0.5)/y/x/((-x)/(-8.8)/(-2.3)+x*(-7.7))*(y-(-1.1))*((-1.7)+(4.5/x)/y)+(x/(-2.8)-(-y))-y)
        let s = (((-0.5)/y/x/((-x)/(-8.8)/(-2.3)+x*(-7.7))*(y-(-1.1))*((-1.7)+(4.5/x)/y)+(x/(-2.8)-(-y))-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-0.5)/q/p/((-p)/(-8.8)/(-2.3)+p*(-7.7))*(q-(-1.1))*((-1.7)+(4.5/p)/q)+(p/(-2.8)-(-q))-q)
            z2 <== ((-0.5)/y/x/((-x)/(-8.8)/(-2.3)+x*(-7.7))*(y-(-1.1))*((-1.7)+(4.5/x)/y)+(x/(-2.8)-(-y))-y)
            wr.tt <| (I 547)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 548
        ctx.print.s "test548"
        //equation: (-5.2)
        //printfn "%d" 549
        ctx.print.s "test549"
        //equation: (-8.1)
        //printfn "%d" 550
        ctx.print.s "test550"
        //equation: ((-y)/(-6.8))
        let s = (((-y)/(-6.8))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)/(-6.8))
            z2 <== ((-y)/(-6.8))
            wr.tt <| (I 550)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 551
        ctx.print.s "test551"
        //equation: (((-x))*(2.2*((-y)*1.3-(-x)*x))+(((-y)/3.6-x)+0.3*(-5.7))-(x/(-1.0)/y*y*(-y)*3.6+(-x)+((-0.2)-x-4.2/x/(-1.6))+(3.2+(-x)+y)))
        let s = ((((-x))*(2.2*((-y)*1.3-(-x)*x))+(((-y)/3.6-x)+0.3*(-5.7))-(x/(-1.0)/y*y*(-y)*3.6+(-x)+((-0.2)-x-4.2/x/(-1.6))+(3.2+(-x)+y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-p))*(2.2*((-q)*1.3-(-p)*p))+(((-q)/3.6-p)+0.3*(-5.7))-(p/(-1.0)/q*q*(-q)*3.6+(-p)+((-0.2)-p-4.2/p/(-1.6))+(3.2+(-p)+q)))
            z2 <== (((-x))*(2.2*((-y)*1.3-(-x)*x))+(((-y)/3.6-x)+0.3*(-5.7))-(x/(-1.0)/y*y*(-y)*3.6+(-x)+((-0.2)-x-4.2/x/(-1.6))+(3.2+(-x)+y)))
            wr.tt <| (I 551)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 552
        ctx.print.s "test552"
        //equation: (x+((x+x+(-x))+x+4.7+0.8))
        let s = ((x+((x+x+(-x))+x+4.7+0.8))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p+((p+p+(-p))+p+4.7+0.8))
            z2 <== (x+((x+x+(-x))+x+4.7+0.8))
            wr.tt <| (I 552)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 553
        ctx.print.s "test553"
        //equation: ((((-7.0)-x/(-y)/2.4+(-1.0))*6.3*0.7-6.7)*8.8*(-x))
        let s = (((((-7.0)-x/(-y)/2.4+(-1.0))*6.3*0.7-6.7)*8.8*(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-7.0)-p/(-q)/2.4+(-1.0))*6.3*0.7-6.7)*8.8*(-p))
            z2 <== ((((-7.0)-x/(-y)/2.4+(-1.0))*6.3*0.7-6.7)*8.8*(-x))
            wr.tt <| (I 553)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 554
        ctx.print.s "test554"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 554)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 555
        ctx.print.s "test555"
        //equation: (-2.8)
        //printfn "%d" 556
        ctx.print.s "test556"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 556)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 557
        ctx.print.s "test557"
        //equation: (-5.7)
        //printfn "%d" 558
        ctx.print.s "test558"
        //equation: 3.8/((-0.8)/y)-x+2.0-2.7/((-3.3)/(-3.8)-(-y)+7.8*(-y)-(3.4*(-1.6)*8.0))+(-x)
        let s = (3.8/((-0.8)/y)-x+2.0-2.7/((-3.3)/(-3.8)-(-y)+7.8*(-y)-(3.4*(-1.6)*8.0))+(-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== 3.8/((-0.8)/q)-p+2.0-2.7/((-3.3)/(-3.8)-(-q)+7.8*(-q)-(3.4*(-1.6)*8.0))+(-p)
            z2 <== 3.8/((-0.8)/y)-x+2.0-2.7/((-3.3)/(-3.8)-(-y)+7.8*(-y)-(3.4*(-1.6)*8.0))+(-x)
            wr.tt <| (I 558)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 559
        ctx.print.s "test559"
        //equation: 6.4
        //printfn "%d" 560
        ctx.print.s "test560"
        //equation: ((-x))
        let s = (((-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p))
            z2 <== ((-x))
            wr.tt <| (I 560)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 561
        ctx.print.s "test561"
        //equation: (((-5.2)-((-y)-0.5-(-y)/5.1-(-1.1))))
        let s = ((((-5.2)-((-y)-0.5-(-y)/5.1-(-1.1))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-5.2)-((-q)-0.5-(-q)/5.1-(-1.1))))
            z2 <== (((-5.2)-((-y)-0.5-(-y)/5.1-(-1.1))))
            wr.tt <| (I 561)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 562
        ctx.print.s "test562"
        //equation: (-0.1)
        //printfn "%d" 563
        ctx.print.s "test563"
        //equation: ((-y)+8.0)
        let s = (((-y)+8.0)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)+8.0)
            z2 <== ((-y)+8.0)
            wr.tt <| (I 563)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 564
        ctx.print.s "test564"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 564)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 565
        ctx.print.s "test565"
        //equation: ((-x)*5.6+((-1.8)-(-1.8)))*1.4/(7.1-((-x)+y)*(-x)+(-6.4)*6.2)
        let s = (((-x)*5.6+((-1.8)-(-1.8)))*1.4/(7.1-((-x)+y)*(-x)+(-6.4)*6.2)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)*5.6+((-1.8)-(-1.8)))*1.4/(7.1-((-p)+q)*(-p)+(-6.4)*6.2)
            z2 <== ((-x)*5.6+((-1.8)-(-1.8)))*1.4/(7.1-((-x)+y)*(-x)+(-6.4)*6.2)
            wr.tt <| (I 565)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 566
        ctx.print.s "test566"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 566)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 567
        ctx.print.s "test567"
        //equation: ((5.8*8.8+(-x)-5.2-x)*(-1.8)*x-(0.8-3.7/1.7+(-0.6)*(y*(-1.0)+(-7.7))+(-5.0)/(4.3-(-x)-y*y))*(y*(-7.6)*(-6.2)+y/4.6))
        let s = (((5.8*8.8+(-x)-5.2-x)*(-1.8)*x-(0.8-3.7/1.7+(-0.6)*(y*(-1.0)+(-7.7))+(-5.0)/(4.3-(-x)-y*y))*(y*(-7.6)*(-6.2)+y/4.6))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((5.8*8.8+(-p)-5.2-p)*(-1.8)*p-(0.8-3.7/1.7+(-0.6)*(q*(-1.0)+(-7.7))+(-5.0)/(4.3-(-p)-q*q))*(q*(-7.6)*(-6.2)+q/4.6))
            z2 <== ((5.8*8.8+(-x)-5.2-x)*(-1.8)*x-(0.8-3.7/1.7+(-0.6)*(y*(-1.0)+(-7.7))+(-5.0)/(4.3-(-x)-y*y))*(y*(-7.6)*(-6.2)+y/4.6))
            wr.tt <| (I 567)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 568
        ctx.print.s "test568"
        //equation: (((-y))*(y)+2.0+y-2.2-(-y)*(-y)-(-x)+((-6.3)*(x+(-x)*(-3.4))+(y-4.7*y))+((y-5.0)+(6.7)/2.8*((-6.5))*(-3.2)))
        let s = ((((-y))*(y)+2.0+y-2.2-(-y)*(-y)-(-x)+((-6.3)*(x+(-x)*(-3.4))+(y-4.7*y))+((y-5.0)+(6.7)/2.8*((-6.5))*(-3.2)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q))*(q)+2.0+q-2.2-(-q)*(-q)-(-p)+((-6.3)*(p+(-p)*(-3.4))+(q-4.7*q))+((q-5.0)+(6.7)/2.8*((-6.5))*(-3.2)))
            z2 <== (((-y))*(y)+2.0+y-2.2-(-y)*(-y)-(-x)+((-6.3)*(x+(-x)*(-3.4))+(y-4.7*y))+((y-5.0)+(6.7)/2.8*((-6.5))*(-3.2)))
            wr.tt <| (I 568)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 569
        ctx.print.s "test569"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 569)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 570
        ctx.print.s "test570"
        //equation: 6.4
        //printfn "%d" 571
        ctx.print.s "test571"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 571)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 572
        ctx.print.s "test572"
        //equation: y/x-(-x)+0.6
        let s = (y/x-(-x)+0.6).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q/p-(-p)+0.6
            z2 <== y/x-(-x)+0.6
            wr.tt <| (I 572)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 573
        ctx.print.s "test573"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 573)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 574
        ctx.print.s "test574"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 574)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 575
        ctx.print.s "test575"
        //equation: ((-2.6)/((x-x-(-8.5))*(-3.4))*(-8.1)*x-(-x)-(-1.2))
        let s = (((-2.6)/((x-x-(-8.5))*(-3.4))*(-8.1)*x-(-x)-(-1.2))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-2.6)/((p-p-(-8.5))*(-3.4))*(-8.1)*p-(-p)-(-1.2))
            z2 <== ((-2.6)/((x-x-(-8.5))*(-3.4))*(-8.1)*x-(-x)-(-1.2))
            wr.tt <| (I 575)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 576
        ctx.print.s "test576"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 576)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 577
        ctx.print.s "test577"
        //equation: x/y
        let s = (x/y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p/q
            z2 <== x/y
            wr.tt <| (I 577)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 578
        ctx.print.s "test578"
        //equation: y*(-y)+(((-3.1)-2.3/8.0+(-3.4)-y)-((-4.3)/(-0.3)*y/(-x)+(-x)))/(-x)-5.2
        let s = (y*(-y)+(((-3.1)-2.3/8.0+(-3.4)-y)-((-4.3)/(-0.3)*y/(-x)+(-x)))/(-x)-5.2).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q*(-q)+(((-3.1)-2.3/8.0+(-3.4)-q)-((-4.3)/(-0.3)*q/(-p)+(-p)))/(-p)-5.2
            z2 <== y*(-y)+(((-3.1)-2.3/8.0+(-3.4)-y)-((-4.3)/(-0.3)*y/(-x)+(-x)))/(-x)-5.2
            wr.tt <| (I 578)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 579
        ctx.print.s "test579"
        //equation: ((-y)*(y-(-x))*(-3.1)*((-y)-x))
        let s = (((-y)*(y-(-x))*(-3.1)*((-y)-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)*(q-(-p))*(-3.1)*((-q)-p))
            z2 <== ((-y)*(y-(-x))*(-3.1)*((-y)-x))
            wr.tt <| (I 579)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 580
        ctx.print.s "test580"
        //equation: (((-y)*y/(6.1+(-4.6))*y-(x/(-6.6))))
        let s = ((((-y)*y/(6.1+(-4.6))*y-(x/(-6.6))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q)*q/(6.1+(-4.6))*q-(p/(-6.6))))
            z2 <== (((-y)*y/(6.1+(-4.6))*y-(x/(-6.6))))
            wr.tt <| (I 580)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 581
        ctx.print.s "test581"
        //equation: 4.5
        //printfn "%d" 582
        ctx.print.s "test582"
        //equation: 3.6
        //printfn "%d" 583
        ctx.print.s "test583"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 583)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 584
        ctx.print.s "test584"
        //equation: ((-y)-((x+(-x)))/((-x)/(-1.4)-x)/(-2.3))
        let s = (((-y)-((x+(-x)))/((-x)/(-1.4)-x)/(-2.3))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)-((p+(-p)))/((-p)/(-1.4)-p)/(-2.3))
            z2 <== ((-y)-((x+(-x)))/((-x)/(-1.4)-x)/(-2.3))
            wr.tt <| (I 584)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 585
        ctx.print.s "test585"
        //equation: 8.6
        //printfn "%d" 586
        ctx.print.s "test586"
        //equation: ((7.5+((-2.2))/y-(y))*7.8*(8.8/2.2*(-y))+((-x)+(-2.2))-((y*5.2+y)))
        let s = (((7.5+((-2.2))/y-(y))*7.8*(8.8/2.2*(-y))+((-x)+(-2.2))-((y*5.2+y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((7.5+((-2.2))/q-(q))*7.8*(8.8/2.2*(-q))+((-p)+(-2.2))-((q*5.2+q)))
            z2 <== ((7.5+((-2.2))/y-(y))*7.8*(8.8/2.2*(-y))+((-x)+(-2.2))-((y*5.2+y)))
            wr.tt <| (I 586)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 587
        ctx.print.s "test587"
        //equation: 4.0
        //printfn "%d" 588
        ctx.print.s "test588"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 588)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 589
        ctx.print.s "test589"
        //equation: (y*(-x)-(2.2))
        let s = ((y*(-x)-(2.2))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q*(-p)-(2.2))
            z2 <== (y*(-x)-(2.2))
            wr.tt <| (I 589)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 590
        ctx.print.s "test590"
        //equation: (-2.2)
        //printfn "%d" 591
        ctx.print.s "test591"
        //equation: ((-4.0)/(-x)*(3.7)+(x+y-(-4.3)-(-y)-x)-((-0.8)/(-7.5)-(-0.8)-(-y)/(-6.2)))+((-y)/0.8*(-0.7)+8.3)
        let s = (((-4.0)/(-x)*(3.7)+(x+y-(-4.3)-(-y)-x)-((-0.8)/(-7.5)-(-0.8)-(-y)/(-6.2)))+((-y)/0.8*(-0.7)+8.3)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-4.0)/(-p)*(3.7)+(p+q-(-4.3)-(-q)-p)-((-0.8)/(-7.5)-(-0.8)-(-q)/(-6.2)))+((-q)/0.8*(-0.7)+8.3)
            z2 <== ((-4.0)/(-x)*(3.7)+(x+y-(-4.3)-(-y)-x)-((-0.8)/(-7.5)-(-0.8)-(-y)/(-6.2)))+((-y)/0.8*(-0.7)+8.3)
            wr.tt <| (I 591)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 592
        ctx.print.s "test592"
        //equation: (-8.3)
        //printfn "%d" 593
        ctx.print.s "test593"
        //equation: ((-y)+x*(-y)-(y/(-3.4)+(-x)+3.7)+(y/y/5.4)*((-y)*y*(-x))+y-((4.3*x/1.1)/1.8/((-y)-x))+((0.7+(-4.3))*((-x)/(-7.5))-(-y)/(-y)+5.7/x))
        let s = (((-y)+x*(-y)-(y/(-3.4)+(-x)+3.7)+(y/y/5.4)*((-y)*y*(-x))+y-((4.3*x/1.1)/1.8/((-y)-x))+((0.7+(-4.3))*((-x)/(-7.5))-(-y)/(-y)+5.7/x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)+p*(-q)-(q/(-3.4)+(-p)+3.7)+(q/q/5.4)*((-q)*q*(-p))+q-((4.3*p/1.1)/1.8/((-q)-p))+((0.7+(-4.3))*((-p)/(-7.5))-(-q)/(-q)+5.7/p))
            z2 <== ((-y)+x*(-y)-(y/(-3.4)+(-x)+3.7)+(y/y/5.4)*((-y)*y*(-x))+y-((4.3*x/1.1)/1.8/((-y)-x))+((0.7+(-4.3))*((-x)/(-7.5))-(-y)/(-y)+5.7/x))
            wr.tt <| (I 593)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 594
        ctx.print.s "test594"
        //equation: (7.2*((-x)-(-y)+(-y)+1.4+(-y))-y/(-0.3)-6.5/(-7.4)-((-y)+4.6+x+(y*y)/3.5)-(6.7)+(-4.1))
        let s = ((7.2*((-x)-(-y)+(-y)+1.4+(-y))-y/(-0.3)-6.5/(-7.4)-((-y)+4.6+x+(y*y)/3.5)-(6.7)+(-4.1))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (7.2*((-p)-(-q)+(-q)+1.4+(-q))-q/(-0.3)-6.5/(-7.4)-((-q)+4.6+p+(q*q)/3.5)-(6.7)+(-4.1))
            z2 <== (7.2*((-x)-(-y)+(-y)+1.4+(-y))-y/(-0.3)-6.5/(-7.4)-((-y)+4.6+x+(y*y)/3.5)-(6.7)+(-4.1))
            wr.tt <| (I 594)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 595
        ctx.print.s "test595"
        //equation: (-y)-((-y)-(-7.4)/x*y*(-y))+((-x))-(-y)
        let s = ((-y)-((-y)-(-7.4)/x*y*(-y))+((-x))-(-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)-((-q)-(-7.4)/p*q*(-q))+((-p))-(-q)
            z2 <== (-y)-((-y)-(-7.4)/x*y*(-y))+((-x))-(-y)
            wr.tt <| (I 595)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 596
        ctx.print.s "test596"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 596)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 597
        ctx.print.s "test597"
        //equation: (5.1)
        //printfn "%d" 598
        ctx.print.s "test598"
        //equation: (((-x)+((-y))+y*y/4.8)+6.3/((-1.0)/0.4))
        let s = ((((-x)+((-y))+y*y/4.8)+6.3/((-1.0)/0.4))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-p)+((-q))+q*q/4.8)+6.3/((-1.0)/0.4))
            z2 <== (((-x)+((-y))+y*y/4.8)+6.3/((-1.0)/0.4))
            wr.tt <| (I 598)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 599
        ctx.print.s "test599"
        //equation: (-3.8)
        //printfn "%d" 600
        ctx.print.s "test600"
        //equation: (((x-(-y)/(-0.5)/(-y))+y/(-x))-y-((3.4/(-8.2)-(-5.5)/(-0.2))))
        let s = ((((x-(-y)/(-0.5)/(-y))+y/(-x))-y-((3.4/(-8.2)-(-5.5)/(-0.2))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((p-(-q)/(-0.5)/(-q))+q/(-p))-q-((3.4/(-8.2)-(-5.5)/(-0.2))))
            z2 <== (((x-(-y)/(-0.5)/(-y))+y/(-x))-y-((3.4/(-8.2)-(-5.5)/(-0.2))))
            wr.tt <| (I 600)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 601
        ctx.print.s "test601"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 601)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 602
        ctx.print.s "test602"
        //equation: (((-x)*y+(-y))-(-y))
        let s = ((((-x)*y+(-y))-(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-p)*q+(-q))-(-q))
            z2 <== (((-x)*y+(-y))-(-y))
            wr.tt <| (I 602)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 603
        ctx.print.s "test603"
        //equation: (5.7/8.4*x)
        let s = ((5.7/8.4*x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (5.7/8.4*p)
            z2 <== (5.7/8.4*x)
            wr.tt <| (I 603)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 604
        ctx.print.s "test604"
        //equation: y+x+(-x)+5.3-(-8.7)*(-y)/(6.3+x-(-8.5))-8.5/8.7-((-5.4)-((-0.0)-(-x)-(-x)*(-x)))/2.7
        let s = (y+x+(-x)+5.3-(-8.7)*(-y)/(6.3+x-(-8.5))-8.5/8.7-((-5.4)-((-0.0)-(-x)-(-x)*(-x)))/2.7).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q+p+(-p)+5.3-(-8.7)*(-q)/(6.3+p-(-8.5))-8.5/8.7-((-5.4)-((-0.0)-(-p)-(-p)*(-p)))/2.7
            z2 <== y+x+(-x)+5.3-(-8.7)*(-y)/(6.3+x-(-8.5))-8.5/8.7-((-5.4)-((-0.0)-(-x)-(-x)*(-x)))/2.7
            wr.tt <| (I 604)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 605
        ctx.print.s "test605"
        //equation: 7.4
        //printfn "%d" 606
        ctx.print.s "test606"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 606)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 607
        ctx.print.s "test607"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 607)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 608
        ctx.print.s "test608"
        //equation: ((y-(-0.4)-7.4/(-y)-y)*(-5.8))
        let s = (((y-(-0.4)-7.4/(-y)-y)*(-5.8))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q-(-0.4)-7.4/(-q)-q)*(-5.8))
            z2 <== ((y-(-0.4)-7.4/(-y)-y)*(-5.8))
            wr.tt <| (I 608)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 609
        ctx.print.s "test609"
        //equation: ((-y)+0.5*((-y)+(-x)+(-8.7)+7.7)+(-0.5)+y*(-y)+6.4-(y+0.5+y/(-x)-y)*((-6.3)/8.5/x)*(-x))
        let s = (((-y)+0.5*((-y)+(-x)+(-8.7)+7.7)+(-0.5)+y*(-y)+6.4-(y+0.5+y/(-x)-y)*((-6.3)/8.5/x)*(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)+0.5*((-q)+(-p)+(-8.7)+7.7)+(-0.5)+q*(-q)+6.4-(q+0.5+q/(-p)-q)*((-6.3)/8.5/p)*(-p))
            z2 <== ((-y)+0.5*((-y)+(-x)+(-8.7)+7.7)+(-0.5)+y*(-y)+6.4-(y+0.5+y/(-x)-y)*((-6.3)/8.5/x)*(-x))
            wr.tt <| (I 609)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 610
        ctx.print.s "test610"
        //equation: (-5.5)
        //printfn "%d" 611
        ctx.print.s "test611"
        //equation: 5.7
        //printfn "%d" 612
        ctx.print.s "test612"
        //equation: (y-6.8*(-0.0)*y*x-7.1/(6.1-3.2/(-2.7)))-(-x)/(((-0.0)/(-y)+(-y)/(-x)*7.8)/(-0.8)+(-x)+(-2.1)/7.3-(-6.3)-(-x)*(-0.6)-(-6.0))-((-2.0)/((-8.5)-5.0*1.1-(-5.0))/y)+y
        let s = ((y-6.8*(-0.0)*y*x-7.1/(6.1-3.2/(-2.7)))-(-x)/(((-0.0)/(-y)+(-y)/(-x)*7.8)/(-0.8)+(-x)+(-2.1)/7.3-(-6.3)-(-x)*(-0.6)-(-6.0))-((-2.0)/((-8.5)-5.0*1.1-(-5.0))/y)+y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q-6.8*(-0.0)*q*p-7.1/(6.1-3.2/(-2.7)))-(-p)/(((-0.0)/(-q)+(-q)/(-p)*7.8)/(-0.8)+(-p)+(-2.1)/7.3-(-6.3)-(-p)*(-0.6)-(-6.0))-((-2.0)/((-8.5)-5.0*1.1-(-5.0))/q)+q
            z2 <== (y-6.8*(-0.0)*y*x-7.1/(6.1-3.2/(-2.7)))-(-x)/(((-0.0)/(-y)+(-y)/(-x)*7.8)/(-0.8)+(-x)+(-2.1)/7.3-(-6.3)-(-x)*(-0.6)-(-6.0))-((-2.0)/((-8.5)-5.0*1.1-(-5.0))/y)+y
            wr.tt <| (I 612)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 613
        ctx.print.s "test613"
        //equation: ((x+(-y))*(-x)/6.6-(-3.6)-(-5.6))
        let s = (((x+(-y))*(-x)/6.6-(-3.6)-(-5.6))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p+(-q))*(-p)/6.6-(-3.6)-(-5.6))
            z2 <== ((x+(-y))*(-x)/6.6-(-3.6)-(-5.6))
            wr.tt <| (I 613)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 614
        ctx.print.s "test614"
        //equation: (-5.7)
        //printfn "%d" 615
        ctx.print.s "test615"
        //equation: (((-x)+(-0.3)*(-x)/((-x))))
        let s = ((((-x)+(-0.3)*(-x)/((-x))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-p)+(-0.3)*(-p)/((-p))))
            z2 <== (((-x)+(-0.3)*(-x)/((-x))))
            wr.tt <| (I 615)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 616
        ctx.print.s "test616"
        //equation: (7.5+(-x)-1.6)
        let s = ((7.5+(-x)-1.6)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (7.5+(-p)-1.6)
            z2 <== (7.5+(-x)-1.6)
            wr.tt <| (I 616)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 617
        ctx.print.s "test617"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 617)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 618
        ctx.print.s "test618"
        //equation: (y*(x/1.2+((-x)-x+(-x))))
        let s = ((y*(x/1.2+((-x)-x+(-x))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q*(p/1.2+((-p)-p+(-p))))
            z2 <== (y*(x/1.2+((-x)-x+(-x))))
            wr.tt <| (I 618)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 619
        ctx.print.s "test619"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 619)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 620
        ctx.print.s "test620"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 620)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 621
        ctx.print.s "test621"
        //equation: (y-(x/y+((-2.0)/(-y)*x+(-5.4)*(-y))/((-0.3)*(-x)+4.5))/(-y)*7.3/(y-(-6.1)-(-x)))
        let s = ((y-(x/y+((-2.0)/(-y)*x+(-5.4)*(-y))/((-0.3)*(-x)+4.5))/(-y)*7.3/(y-(-6.1)-(-x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q-(p/q+((-2.0)/(-q)*p+(-5.4)*(-q))/((-0.3)*(-p)+4.5))/(-q)*7.3/(q-(-6.1)-(-p)))
            z2 <== (y-(x/y+((-2.0)/(-y)*x+(-5.4)*(-y))/((-0.3)*(-x)+4.5))/(-y)*7.3/(y-(-6.1)-(-x)))
            wr.tt <| (I 621)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 622
        ctx.print.s "test622"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 622)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 623
        ctx.print.s "test623"
        //equation: (-2.0)
        //printfn "%d" 624
        ctx.print.s "test624"
        //equation: ((-4.4)*((x+0.3/y+(-y)-(-x))-(-y)-(y)/(-0.0)+((-2.3)/x))+(-x))
        let s = (((-4.4)*((x+0.3/y+(-y)-(-x))-(-y)-(y)/(-0.0)+((-2.3)/x))+(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-4.4)*((p+0.3/q+(-q)-(-p))-(-q)-(q)/(-0.0)+((-2.3)/p))+(-p))
            z2 <== ((-4.4)*((x+0.3/y+(-y)-(-x))-(-y)-(y)/(-0.0)+((-2.3)/x))+(-x))
            wr.tt <| (I 624)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 625
        ctx.print.s "test625"
        //equation: x-(-x)/(-x)*0.0+((-x)-x+2.0*x)/(-8.4)+((-5.5))/x
        let s = (x-(-x)/(-x)*0.0+((-x)-x+2.0*x)/(-8.4)+((-5.5))/x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p-(-p)/(-p)*0.0+((-p)-p+2.0*p)/(-8.4)+((-5.5))/p
            z2 <== x-(-x)/(-x)*0.0+((-x)-x+2.0*x)/(-8.4)+((-5.5))/x
            wr.tt <| (I 625)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 626
        ctx.print.s "test626"
        //equation: ((8.4-((-y)*x+(-6.2)*(-x)))*(-x)+(y+(-7.5)/y*y)*(y)/x*(-4.3)+(-y)-(-3.8)/(-8.6)-1.6)
        let s = (((8.4-((-y)*x+(-6.2)*(-x)))*(-x)+(y+(-7.5)/y*y)*(y)/x*(-4.3)+(-y)-(-3.8)/(-8.6)-1.6)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((8.4-((-q)*p+(-6.2)*(-p)))*(-p)+(q+(-7.5)/q*q)*(q)/p*(-4.3)+(-q)-(-3.8)/(-8.6)-1.6)
            z2 <== ((8.4-((-y)*x+(-6.2)*(-x)))*(-x)+(y+(-7.5)/y*y)*(y)/x*(-4.3)+(-y)-(-3.8)/(-8.6)-1.6)
            wr.tt <| (I 626)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 627
        ctx.print.s "test627"
        //equation: ((x)+((-5.6))-(-4.4)*3.1)
        let s = (((x)+((-5.6))-(-4.4)*3.1)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p)+((-5.6))-(-4.4)*3.1)
            z2 <== ((x)+((-5.6))-(-4.4)*3.1)
            wr.tt <| (I 627)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 628
        ctx.print.s "test628"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 628)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 629
        ctx.print.s "test629"
        //equation: (x)
        let s = ((x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p)
            z2 <== (x)
            wr.tt <| (I 629)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 630
        ctx.print.s "test630"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 630)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 631
        ctx.print.s "test631"
        //equation: (((x+3.3*x+x))+((-x)/(2.5*4.3/(-y)-x+y))*((-2.0)+3.4+5.8-((-4.5)+2.8+(-7.4)/3.2*y))/((y*(-y)/(-8.2))+(-5.4)/(1.7)/((-4.2)-x)*(4.1-(-x)-6.4-5.8/(-y)))*(-x))
        let s = ((((x+3.3*x+x))+((-x)/(2.5*4.3/(-y)-x+y))*((-2.0)+3.4+5.8-((-4.5)+2.8+(-7.4)/3.2*y))/((y*(-y)/(-8.2))+(-5.4)/(1.7)/((-4.2)-x)*(4.1-(-x)-6.4-5.8/(-y)))*(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((p+3.3*p+p))+((-p)/(2.5*4.3/(-q)-p+q))*((-2.0)+3.4+5.8-((-4.5)+2.8+(-7.4)/3.2*q))/((q*(-q)/(-8.2))+(-5.4)/(1.7)/((-4.2)-p)*(4.1-(-p)-6.4-5.8/(-q)))*(-p))
            z2 <== (((x+3.3*x+x))+((-x)/(2.5*4.3/(-y)-x+y))*((-2.0)+3.4+5.8-((-4.5)+2.8+(-7.4)/3.2*y))/((y*(-y)/(-8.2))+(-5.4)/(1.7)/((-4.2)-x)*(4.1-(-x)-6.4-5.8/(-y)))*(-x))
            wr.tt <| (I 631)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 632
        ctx.print.s "test632"
        //equation: 4.0/y
        let s = (4.0/y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== 4.0/q
            z2 <== 4.0/y
            wr.tt <| (I 632)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 633
        ctx.print.s "test633"
        //equation: (8.0/((0.1)-(x-(-x)-(-x)-5.8*(-y))-(-3.2)+(-x))+x/y+(-2.1)/3.6+(8.3/(-5.4)/(-2.0)*x/x)+6.0/(-6.0)/x-x)
        let s = ((8.0/((0.1)-(x-(-x)-(-x)-5.8*(-y))-(-3.2)+(-x))+x/y+(-2.1)/3.6+(8.3/(-5.4)/(-2.0)*x/x)+6.0/(-6.0)/x-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (8.0/((0.1)-(p-(-p)-(-p)-5.8*(-q))-(-3.2)+(-p))+p/q+(-2.1)/3.6+(8.3/(-5.4)/(-2.0)*p/p)+6.0/(-6.0)/p-p)
            z2 <== (8.0/((0.1)-(x-(-x)-(-x)-5.8*(-y))-(-3.2)+(-x))+x/y+(-2.1)/3.6+(8.3/(-5.4)/(-2.0)*x/x)+6.0/(-6.0)/x-x)
            wr.tt <| (I 633)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 634
        ctx.print.s "test634"
        //equation: (-1.5)
        //printfn "%d" 635
        ctx.print.s "test635"
        //equation: ((5.2+y/2.2/(-y))+(x-(-8.2)*7.5+(-y)+(-8.5))+(-6.7)/(-7.0)*2.3+(-x)-x-y+(-2.8))
        let s = (((5.2+y/2.2/(-y))+(x-(-8.2)*7.5+(-y)+(-8.5))+(-6.7)/(-7.0)*2.3+(-x)-x-y+(-2.8))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((5.2+q/2.2/(-q))+(p-(-8.2)*7.5+(-q)+(-8.5))+(-6.7)/(-7.0)*2.3+(-p)-p-q+(-2.8))
            z2 <== ((5.2+y/2.2/(-y))+(x-(-8.2)*7.5+(-y)+(-8.5))+(-6.7)/(-7.0)*2.3+(-x)-x-y+(-2.8))
            wr.tt <| (I 635)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 636
        ctx.print.s "test636"
        //equation: 1.3+x/(-2.1)*(-x)*y+(-x)/(-x)+(-x)+((0.1-(-x))-(y/x+(-8.4))/(-6.4)/((-y)))+(((-5.8)/(-6.4)-(-y))+(-x)/2.6*x/(-3.8)/((-y)/(-5.8))/(-y))
        let s = (1.3+x/(-2.1)*(-x)*y+(-x)/(-x)+(-x)+((0.1-(-x))-(y/x+(-8.4))/(-6.4)/((-y)))+(((-5.8)/(-6.4)-(-y))+(-x)/2.6*x/(-3.8)/((-y)/(-5.8))/(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== 1.3+p/(-2.1)*(-p)*q+(-p)/(-p)+(-p)+((0.1-(-p))-(q/p+(-8.4))/(-6.4)/((-q)))+(((-5.8)/(-6.4)-(-q))+(-p)/2.6*p/(-3.8)/((-q)/(-5.8))/(-q))
            z2 <== 1.3+x/(-2.1)*(-x)*y+(-x)/(-x)+(-x)+((0.1-(-x))-(y/x+(-8.4))/(-6.4)/((-y)))+(((-5.8)/(-6.4)-(-y))+(-x)/2.6*x/(-3.8)/((-y)/(-5.8))/(-y))
            wr.tt <| (I 636)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 637
        ctx.print.s "test637"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 637)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 638
        ctx.print.s "test638"
        //equation: (((-2.6)+(y+(-6.4)/(-5.6))+(6.8)*y/(-y))/((-6.7)/(-x)/(6.1-4.6-(-y)*x))+((y-x+(-y)))-(-y))
        let s = ((((-2.6)+(y+(-6.4)/(-5.6))+(6.8)*y/(-y))/((-6.7)/(-x)/(6.1-4.6-(-y)*x))+((y-x+(-y)))-(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-2.6)+(q+(-6.4)/(-5.6))+(6.8)*q/(-q))/((-6.7)/(-p)/(6.1-4.6-(-q)*p))+((q-p+(-q)))-(-q))
            z2 <== (((-2.6)+(y+(-6.4)/(-5.6))+(6.8)*y/(-y))/((-6.7)/(-x)/(6.1-4.6-(-y)*x))+((y-x+(-y)))-(-y))
            wr.tt <| (I 638)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 639
        ctx.print.s "test639"
        //equation: (5.6+y+((-0.4))-(2.3+3.5-(-x)*x-(-y))*(-5.8)+x)
        let s = ((5.6+y+((-0.4))-(2.3+3.5-(-x)*x-(-y))*(-5.8)+x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (5.6+q+((-0.4))-(2.3+3.5-(-p)*p-(-q))*(-5.8)+p)
            z2 <== (5.6+y+((-0.4))-(2.3+3.5-(-x)*x-(-y))*(-5.8)+x)
            wr.tt <| (I 639)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 640
        ctx.print.s "test640"
        //equation: ((x/(5.7/(-6.8)*(-6.2)+0.6*(-y)))*(-x)+((-x)/1.5-(-y)/(x-3.1-y/(-y))+((-y)/1.7*(-4.3))))
        let s = (((x/(5.7/(-6.8)*(-6.2)+0.6*(-y)))*(-x)+((-x)/1.5-(-y)/(x-3.1-y/(-y))+((-y)/1.7*(-4.3))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p/(5.7/(-6.8)*(-6.2)+0.6*(-q)))*(-p)+((-p)/1.5-(-q)/(p-3.1-q/(-q))+((-q)/1.7*(-4.3))))
            z2 <== ((x/(5.7/(-6.8)*(-6.2)+0.6*(-y)))*(-x)+((-x)/1.5-(-y)/(x-3.1-y/(-y))+((-y)/1.7*(-4.3))))
            wr.tt <| (I 640)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 641
        ctx.print.s "test641"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 641)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 642
        ctx.print.s "test642"
        //equation: 7.4
        //printfn "%d" 643
        ctx.print.s "test643"
        //equation: (4.2-y/(7.3-y/(-x))*(-8.8)-((-8.0)/y*y*(-y)))
        let s = ((4.2-y/(7.3-y/(-x))*(-8.8)-((-8.0)/y*y*(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (4.2-q/(7.3-q/(-p))*(-8.8)-((-8.0)/q*q*(-q)))
            z2 <== (4.2-y/(7.3-y/(-x))*(-8.8)-((-8.0)/y*y*(-y)))
            wr.tt <| (I 643)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 644
        ctx.print.s "test644"
        //equation: (3.4)
        //printfn "%d" 645
        ctx.print.s "test645"
        //equation: (((-4.0)*(x/x+(-y))-((-6.8)/(-y))*(x))-((x/8.3/(-y))))
        let s = ((((-4.0)*(x/x+(-y))-((-6.8)/(-y))*(x))-((x/8.3/(-y))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-4.0)*(p/p+(-q))-((-6.8)/(-q))*(p))-((p/8.3/(-q))))
            z2 <== (((-4.0)*(x/x+(-y))-((-6.8)/(-y))*(x))-((x/8.3/(-y))))
            wr.tt <| (I 645)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 646
        ctx.print.s "test646"
        //equation: (((8.3+(-0.3)*(-y))+y+(-x)*(-y)-3.0/(8.6/(-5.1)*x/(-y)))+((x-(-x))/y+((-y)+y/3.4*(-y)))+(-6.8))
        let s = ((((8.3+(-0.3)*(-y))+y+(-x)*(-y)-3.0/(8.6/(-5.1)*x/(-y)))+((x-(-x))/y+((-y)+y/3.4*(-y)))+(-6.8))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((8.3+(-0.3)*(-q))+q+(-p)*(-q)-3.0/(8.6/(-5.1)*p/(-q)))+((p-(-p))/q+((-q)+q/3.4*(-q)))+(-6.8))
            z2 <== (((8.3+(-0.3)*(-y))+y+(-x)*(-y)-3.0/(8.6/(-5.1)*x/(-y)))+((x-(-x))/y+((-y)+y/3.4*(-y)))+(-6.8))
            wr.tt <| (I 646)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 647
        ctx.print.s "test647"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 647)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 648
        ctx.print.s "test648"
        //equation: (((-7.6)-1.5*x)*(-6.2))
        let s = ((((-7.6)-1.5*x)*(-6.2))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-7.6)-1.5*p)*(-6.2))
            z2 <== (((-7.6)-1.5*x)*(-6.2))
            wr.tt <| (I 648)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 649
        ctx.print.s "test649"
        //equation: 1.5
        //printfn "%d" 650
        ctx.print.s "test650"
        //equation: 6.5
        //printfn "%d" 651
        ctx.print.s "test651"
        //equation: 1.4
        //printfn "%d" 652
        ctx.print.s "test652"
        //equation: ((x-(-x)-(-5.1)+x/(-3.7))*(-x))/(x*y)/7.1+y+(-3.4)
        let s = (((x-(-x)-(-5.1)+x/(-3.7))*(-x))/(x*y)/7.1+y+(-3.4)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p-(-p)-(-5.1)+p/(-3.7))*(-p))/(p*q)/7.1+q+(-3.4)
            z2 <== ((x-(-x)-(-5.1)+x/(-3.7))*(-x))/(x*y)/7.1+y+(-3.4)
            wr.tt <| (I 652)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 653
        ctx.print.s "test653"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 653)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 654
        ctx.print.s "test654"
        //equation: (2.7)
        //printfn "%d" 655
        ctx.print.s "test655"
        //equation: (((5.0/(-1.0)+(-y)/(-y)/(-y))/(1.4+8.0)/((-0.2)+y*(-y))/(x*y+y)*(-y)+4.3+(-x)/(-7.4)-y)+(-2.3)/8.2/(5.6*(-x)/4.0+x)+(-1.5))
        let s = ((((5.0/(-1.0)+(-y)/(-y)/(-y))/(1.4+8.0)/((-0.2)+y*(-y))/(x*y+y)*(-y)+4.3+(-x)/(-7.4)-y)+(-2.3)/8.2/(5.6*(-x)/4.0+x)+(-1.5))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((5.0/(-1.0)+(-q)/(-q)/(-q))/(1.4+8.0)/((-0.2)+q*(-q))/(p*q+q)*(-q)+4.3+(-p)/(-7.4)-q)+(-2.3)/8.2/(5.6*(-p)/4.0+p)+(-1.5))
            z2 <== (((5.0/(-1.0)+(-y)/(-y)/(-y))/(1.4+8.0)/((-0.2)+y*(-y))/(x*y+y)*(-y)+4.3+(-x)/(-7.4)-y)+(-2.3)/8.2/(5.6*(-x)/4.0+x)+(-1.5))
            wr.tt <| (I 655)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 656
        ctx.print.s "test656"
        //equation: 2.8
        //printfn "%d" 657
        ctx.print.s "test657"
        //equation: (((y*(-0.3)-7.1)/(-5.2)+(-7.8)+(x)*((-3.0)/y*(-x)))+6.4*7.6)
        let s = ((((y*(-0.3)-7.1)/(-5.2)+(-7.8)+(x)*((-3.0)/y*(-x)))+6.4*7.6)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((q*(-0.3)-7.1)/(-5.2)+(-7.8)+(p)*((-3.0)/q*(-p)))+6.4*7.6)
            z2 <== (((y*(-0.3)-7.1)/(-5.2)+(-7.8)+(x)*((-3.0)/y*(-x)))+6.4*7.6)
            wr.tt <| (I 657)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 658
        ctx.print.s "test658"
        //equation: (2.4*(2.6-(-0.1))*(-2.0))
        //printfn "%d" 659
        ctx.print.s "test659"
        //equation: (((-5.7)+(x*7.4-4.1)))
        let s = ((((-5.7)+(x*7.4-4.1)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-5.7)+(p*7.4-4.1)))
            z2 <== (((-5.7)+(x*7.4-4.1)))
            wr.tt <| (I 659)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 660
        ctx.print.s "test660"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 660)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 661
        ctx.print.s "test661"
        //equation: 7.4
        //printfn "%d" 662
        ctx.print.s "test662"
        //equation: ((8.4-3.4+(8.8/y+x)-((-0.1)+y*(-x)-(-y)/y)-y)*(4.7-((-1.3)-1.5-y-1.4/(-6.7))*((-0.6)+(-2.5)+(-x)))-(-x)+((-y)+(-x)-y/(y*(-7.0))*(-y)))
        let s = (((8.4-3.4+(8.8/y+x)-((-0.1)+y*(-x)-(-y)/y)-y)*(4.7-((-1.3)-1.5-y-1.4/(-6.7))*((-0.6)+(-2.5)+(-x)))-(-x)+((-y)+(-x)-y/(y*(-7.0))*(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((8.4-3.4+(8.8/q+p)-((-0.1)+q*(-p)-(-q)/q)-q)*(4.7-((-1.3)-1.5-q-1.4/(-6.7))*((-0.6)+(-2.5)+(-p)))-(-p)+((-q)+(-p)-q/(q*(-7.0))*(-q)))
            z2 <== ((8.4-3.4+(8.8/y+x)-((-0.1)+y*(-x)-(-y)/y)-y)*(4.7-((-1.3)-1.5-y-1.4/(-6.7))*((-0.6)+(-2.5)+(-x)))-(-x)+((-y)+(-x)-y/(y*(-7.0))*(-y)))
            wr.tt <| (I 662)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 663
        ctx.print.s "test663"
        //equation: (-7.0)
        //printfn "%d" 664
        ctx.print.s "test664"
        //equation: ((-y)-(y/x))
        let s = (((-y)-(y/x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)-(q/p))
            z2 <== ((-y)-(y/x))
            wr.tt <| (I 664)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 665
        ctx.print.s "test665"
        //equation: 1.1
        //printfn "%d" 666
        ctx.print.s "test666"
        //equation: (-6.7)*(-1.2)+(((-0.5)-(-y))+0.0*(-y)*(x*8.5+x)/(0.6+(-y)))*x
        let s = ((-6.7)*(-1.2)+(((-0.5)-(-y))+0.0*(-y)*(x*8.5+x)/(0.6+(-y)))*x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-6.7)*(-1.2)+(((-0.5)-(-q))+0.0*(-q)*(p*8.5+p)/(0.6+(-q)))*p
            z2 <== (-6.7)*(-1.2)+(((-0.5)-(-y))+0.0*(-y)*(x*8.5+x)/(0.6+(-y)))*x
            wr.tt <| (I 666)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 667
        ctx.print.s "test667"
        //equation: (((-4.1)-7.8))
        //printfn "%d" 668
        ctx.print.s "test668"
        //equation: ((2.8-x-(-x)))
        let s = (((2.8-x-(-x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((2.8-p-(-p)))
            z2 <== ((2.8-x-(-x)))
            wr.tt <| (I 668)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 669
        ctx.print.s "test669"
        //equation: (((-1.0)*((-8.5)*8.1)+((-5.2)/6.0-4.0/y/y))-5.1)
        let s = ((((-1.0)*((-8.5)*8.1)+((-5.2)/6.0-4.0/y/y))-5.1)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-1.0)*((-8.5)*8.1)+((-5.2)/6.0-4.0/q/q))-5.1)
            z2 <== (((-1.0)*((-8.5)*8.1)+((-5.2)/6.0-4.0/y/y))-5.1)
            wr.tt <| (I 669)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 670
        ctx.print.s "test670"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 670)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 671
        ctx.print.s "test671"
        //equation: ((-x)*(-4.2)*y-((y+(-3.1))-4.1+(-y)/(-1.8)+3.7)*(-6.0)*(-1.3))
        let s = (((-x)*(-4.2)*y-((y+(-3.1))-4.1+(-y)/(-1.8)+3.7)*(-6.0)*(-1.3))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)*(-4.2)*q-((q+(-3.1))-4.1+(-q)/(-1.8)+3.7)*(-6.0)*(-1.3))
            z2 <== ((-x)*(-4.2)*y-((y+(-3.1))-4.1+(-y)/(-1.8)+3.7)*(-6.0)*(-1.3))
            wr.tt <| (I 671)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 672
        ctx.print.s "test672"
        //equation: (-0.1)
        //printfn "%d" 673
        ctx.print.s "test673"
        //equation: 3.1
        //printfn "%d" 674
        ctx.print.s "test674"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 674)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 675
        ctx.print.s "test675"
        //equation: 6.4
        //printfn "%d" 676
        ctx.print.s "test676"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 676)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 677
        ctx.print.s "test677"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 677)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 678
        ctx.print.s "test678"
        //equation: ((((-x)*(-5.4)*(-y)+(-0.5)*6.5)-(-0.7)*y)+7.7+((y/(-y)+(-0.1)-y))*(-x)-(x+((-7.1)-(-0.7))))
        let s = (((((-x)*(-5.4)*(-y)+(-0.5)*6.5)-(-0.7)*y)+7.7+((y/(-y)+(-0.1)-y))*(-x)-(x+((-7.1)-(-0.7))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-p)*(-5.4)*(-q)+(-0.5)*6.5)-(-0.7)*q)+7.7+((q/(-q)+(-0.1)-q))*(-p)-(p+((-7.1)-(-0.7))))
            z2 <== ((((-x)*(-5.4)*(-y)+(-0.5)*6.5)-(-0.7)*y)+7.7+((y/(-y)+(-0.1)-y))*(-x)-(x+((-7.1)-(-0.7))))
            wr.tt <| (I 678)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 679
        ctx.print.s "test679"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 679)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 680
        ctx.print.s "test680"
        //equation: 1.4
        //printfn "%d" 681
        ctx.print.s "test681"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 681)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 682
        ctx.print.s "test682"
        //equation: (y)
        let s = ((y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q)
            z2 <== (y)
            wr.tt <| (I 682)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 683
        ctx.print.s "test683"
        //equation: (-2.6)
        //printfn "%d" 684
        ctx.print.s "test684"
        //equation: 6.7
        //printfn "%d" 685
        ctx.print.s "test685"
        //equation: 6.3
        //printfn "%d" 686
        ctx.print.s "test686"
        //equation: 8.6
        //printfn "%d" 687
        ctx.print.s "test687"
        //equation: 1.4
        //printfn "%d" 688
        ctx.print.s "test688"
        //equation: ((-0.0)*(-2.5)-(-5.6)-((-2.8)-y-(-y)/y*((-x)-(-7.2)/4.4-(-x))-(y/(-7.4)*(-1.0)/(-y)-(-6.1))+((-x)*8.7/y+4.6-y)))
        let s = (((-0.0)*(-2.5)-(-5.6)-((-2.8)-y-(-y)/y*((-x)-(-7.2)/4.4-(-x))-(y/(-7.4)*(-1.0)/(-y)-(-6.1))+((-x)*8.7/y+4.6-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-0.0)*(-2.5)-(-5.6)-((-2.8)-q-(-q)/q*((-p)-(-7.2)/4.4-(-p))-(q/(-7.4)*(-1.0)/(-q)-(-6.1))+((-p)*8.7/q+4.6-q)))
            z2 <== ((-0.0)*(-2.5)-(-5.6)-((-2.8)-y-(-y)/y*((-x)-(-7.2)/4.4-(-x))-(y/(-7.4)*(-1.0)/(-y)-(-6.1))+((-x)*8.7/y+4.6-y)))
            wr.tt <| (I 688)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 689
        ctx.print.s "test689"
        //equation: 5.6
        //printfn "%d" 690
        ctx.print.s "test690"
        //equation: (-y)*(((-x))+0.7-6.7/(-y)-(-y)*(-0.0)-2.6-(-x)*(7.5-(-x)))+x
        let s = ((-y)*(((-x))+0.7-6.7/(-y)-(-y)*(-0.0)-2.6-(-x)*(7.5-(-x)))+x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)*(((-p))+0.7-6.7/(-q)-(-q)*(-0.0)-2.6-(-p)*(7.5-(-p)))+p
            z2 <== (-y)*(((-x))+0.7-6.7/(-y)-(-y)*(-0.0)-2.6-(-x)*(7.5-(-x)))+x
            wr.tt <| (I 690)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 691
        ctx.print.s "test691"
        //equation: (7.3/(((-1.0)-y-(-2.1)+x-(-y))-(-5.8)/2.8)*(y+(-y)*x)/(((-x)+(-5.5)/(-5.6))*(-x)-(-1.3))*(x/(-y)))
        let s = ((7.3/(((-1.0)-y-(-2.1)+x-(-y))-(-5.8)/2.8)*(y+(-y)*x)/(((-x)+(-5.5)/(-5.6))*(-x)-(-1.3))*(x/(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (7.3/(((-1.0)-q-(-2.1)+p-(-q))-(-5.8)/2.8)*(q+(-q)*p)/(((-p)+(-5.5)/(-5.6))*(-p)-(-1.3))*(p/(-q)))
            z2 <== (7.3/(((-1.0)-y-(-2.1)+x-(-y))-(-5.8)/2.8)*(y+(-y)*x)/(((-x)+(-5.5)/(-5.6))*(-x)-(-1.3))*(x/(-y)))
            wr.tt <| (I 691)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 692
        ctx.print.s "test692"
        //equation: (x*((8.5+(-x)-x)+(-x)))
        let s = ((x*((8.5+(-x)-x)+(-x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p*((8.5+(-p)-p)+(-p)))
            z2 <== (x*((8.5+(-x)-x)+(-x)))
            wr.tt <| (I 692)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 693
        ctx.print.s "test693"
        //equation: (((1.8*(-7.0)*(-6.7)+(-1.6))+(1.8*3.7*0.8)-2.2)-(-y))
        let s = ((((1.8*(-7.0)*(-6.7)+(-1.6))+(1.8*3.7*0.8)-2.2)-(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((1.8*(-7.0)*(-6.7)+(-1.6))+(1.8*3.7*0.8)-2.2)-(-q))
            z2 <== (((1.8*(-7.0)*(-6.7)+(-1.6))+(1.8*3.7*0.8)-2.2)-(-y))
            wr.tt <| (I 693)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 694
        ctx.print.s "test694"
        //equation: 6.7
        //printfn "%d" 695
        ctx.print.s "test695"
        //equation: ((-7.7)+(x/((-x)-x*(-2.6)*x)*(-4.8)+((-x)*3.7))+5.4*(-x)+x/(-4.0)*(3.8/y+(-7.1)+(-3.8)+(-2.8))-(-2.4))
        let s = (((-7.7)+(x/((-x)-x*(-2.6)*x)*(-4.8)+((-x)*3.7))+5.4*(-x)+x/(-4.0)*(3.8/y+(-7.1)+(-3.8)+(-2.8))-(-2.4))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-7.7)+(p/((-p)-p*(-2.6)*p)*(-4.8)+((-p)*3.7))+5.4*(-p)+p/(-4.0)*(3.8/q+(-7.1)+(-3.8)+(-2.8))-(-2.4))
            z2 <== ((-7.7)+(x/((-x)-x*(-2.6)*x)*(-4.8)+((-x)*3.7))+5.4*(-x)+x/(-4.0)*(3.8/y+(-7.1)+(-3.8)+(-2.8))-(-2.4))
            wr.tt <| (I 695)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 696
        ctx.print.s "test696"
        //equation: (((-y)-(-0.8)*((-x)*(-7.6)/1.3+(-x))-(-8.7)+(x/(-y)-(-1.2)+y)/(-4.6))/(-4.5)-(-y)/5.2)
        let s = ((((-y)-(-0.8)*((-x)*(-7.6)/1.3+(-x))-(-8.7)+(x/(-y)-(-1.2)+y)/(-4.6))/(-4.5)-(-y)/5.2)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q)-(-0.8)*((-p)*(-7.6)/1.3+(-p))-(-8.7)+(p/(-q)-(-1.2)+q)/(-4.6))/(-4.5)-(-q)/5.2)
            z2 <== (((-y)-(-0.8)*((-x)*(-7.6)/1.3+(-x))-(-8.7)+(x/(-y)-(-1.2)+y)/(-4.6))/(-4.5)-(-y)/5.2)
            wr.tt <| (I 696)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 697
        ctx.print.s "test697"
        //equation: (5.6)*5.6+(-x)/1.3
        let s = ((5.6)*5.6+(-x)/1.3).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (5.6)*5.6+(-p)/1.3
            z2 <== (5.6)*5.6+(-x)/1.3
            wr.tt <| (I 697)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 698
        ctx.print.s "test698"
        //equation: ((y/((-x)+(-0.5)))*6.7/((-3.0)))
        let s = (((y/((-x)+(-0.5)))*6.7/((-3.0)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q/((-p)+(-0.5)))*6.7/((-3.0)))
            z2 <== ((y/((-x)+(-0.5)))*6.7/((-3.0)))
            wr.tt <| (I 698)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 699
        ctx.print.s "test699"
        //equation: ((x*(-y)-y*((-7.7)-(-4.2))))
        let s = (((x*(-y)-y*((-7.7)-(-4.2))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p*(-q)-q*((-7.7)-(-4.2))))
            z2 <== ((x*(-y)-y*((-7.7)-(-4.2))))
            wr.tt <| (I 699)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 700
        ctx.print.s "test700"
        //equation: 7.8
        //printfn "%d" 701
        ctx.print.s "test701"
        //equation: (((x)*(-7.7)-(-4.0))/(8.4/((-x)-x+(-7.3)-x-(-y))+((-7.6)-1.5-(-y))+((-4.0)/8.8/(-5.0)+x-y)+((-4.6)))+((-y)*(-6.2)-(-3.7)-(y-4.4))*(-x))
        let s = ((((x)*(-7.7)-(-4.0))/(8.4/((-x)-x+(-7.3)-x-(-y))+((-7.6)-1.5-(-y))+((-4.0)/8.8/(-5.0)+x-y)+((-4.6)))+((-y)*(-6.2)-(-3.7)-(y-4.4))*(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((p)*(-7.7)-(-4.0))/(8.4/((-p)-p+(-7.3)-p-(-q))+((-7.6)-1.5-(-q))+((-4.0)/8.8/(-5.0)+p-q)+((-4.6)))+((-q)*(-6.2)-(-3.7)-(q-4.4))*(-p))
            z2 <== (((x)*(-7.7)-(-4.0))/(8.4/((-x)-x+(-7.3)-x-(-y))+((-7.6)-1.5-(-y))+((-4.0)/8.8/(-5.0)+x-y)+((-4.6)))+((-y)*(-6.2)-(-3.7)-(y-4.4))*(-x))
            wr.tt <| (I 701)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 702
        ctx.print.s "test702"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 702)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 703
        ctx.print.s "test703"
        //equation: (2.4+5.6-(-4.4)*7.6)
        //printfn "%d" 704
        ctx.print.s "test704"
        //equation: 0.6
        //printfn "%d" 705
        ctx.print.s "test705"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 705)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 706
        ctx.print.s "test706"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 706)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 707
        ctx.print.s "test707"
        //equation: 4.0
        //printfn "%d" 708
        ctx.print.s "test708"
        //equation: 2.8
        //printfn "%d" 709
        ctx.print.s "test709"
        //equation: (((-1.4)/y+(-y)*4.1)+(((-2.2)-(-x)*(-8.1)+y)/6.6/(-6.6)/(-3.3)*(-y)/3.7/x+(-y))-(-y)*(y/(-x)*(y)*4.1)/(-y))
        let s = ((((-1.4)/y+(-y)*4.1)+(((-2.2)-(-x)*(-8.1)+y)/6.6/(-6.6)/(-3.3)*(-y)/3.7/x+(-y))-(-y)*(y/(-x)*(y)*4.1)/(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-1.4)/q+(-q)*4.1)+(((-2.2)-(-p)*(-8.1)+q)/6.6/(-6.6)/(-3.3)*(-q)/3.7/p+(-q))-(-q)*(q/(-p)*(q)*4.1)/(-q))
            z2 <== (((-1.4)/y+(-y)*4.1)+(((-2.2)-(-x)*(-8.1)+y)/6.6/(-6.6)/(-3.3)*(-y)/3.7/x+(-y))-(-y)*(y/(-x)*(y)*4.1)/(-y))
            wr.tt <| (I 709)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 710
        ctx.print.s "test710"
        //equation: ((x-(4.2*y*(-x))+(6.1/(-x)+x)*(-x)*(-y)*x+(-y)/(-3.6))*(-y)+((-x)/x*((-y)/3.0/0.6/7.3)*(-8.2)*(-5.2)/y+6.5-x*y)+((-1.4)-6.7))
        let s = (((x-(4.2*y*(-x))+(6.1/(-x)+x)*(-x)*(-y)*x+(-y)/(-3.6))*(-y)+((-x)/x*((-y)/3.0/0.6/7.3)*(-8.2)*(-5.2)/y+6.5-x*y)+((-1.4)-6.7))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p-(4.2*q*(-p))+(6.1/(-p)+p)*(-p)*(-q)*p+(-q)/(-3.6))*(-q)+((-p)/p*((-q)/3.0/0.6/7.3)*(-8.2)*(-5.2)/q+6.5-p*q)+((-1.4)-6.7))
            z2 <== ((x-(4.2*y*(-x))+(6.1/(-x)+x)*(-x)*(-y)*x+(-y)/(-3.6))*(-y)+((-x)/x*((-y)/3.0/0.6/7.3)*(-8.2)*(-5.2)/y+6.5-x*y)+((-1.4)-6.7))
            wr.tt <| (I 710)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 711
        ctx.print.s "test711"
        //equation: ((-7.7)*(3.2*((-y)*(-y)+6.7)+(-3.6))+(((-y)/(-y))+(-1.2)/(-y))+(-6.3)-(((-1.6)/(-4.7)*(-y)-8.8/(-x))/y*(8.1+(-5.4)/y/(-y)/(-0.6))))
        let s = (((-7.7)*(3.2*((-y)*(-y)+6.7)+(-3.6))+(((-y)/(-y))+(-1.2)/(-y))+(-6.3)-(((-1.6)/(-4.7)*(-y)-8.8/(-x))/y*(8.1+(-5.4)/y/(-y)/(-0.6))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-7.7)*(3.2*((-q)*(-q)+6.7)+(-3.6))+(((-q)/(-q))+(-1.2)/(-q))+(-6.3)-(((-1.6)/(-4.7)*(-q)-8.8/(-p))/q*(8.1+(-5.4)/q/(-q)/(-0.6))))
            z2 <== ((-7.7)*(3.2*((-y)*(-y)+6.7)+(-3.6))+(((-y)/(-y))+(-1.2)/(-y))+(-6.3)-(((-1.6)/(-4.7)*(-y)-8.8/(-x))/y*(8.1+(-5.4)/y/(-y)/(-0.6))))
            wr.tt <| (I 711)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 712
        ctx.print.s "test712"
        //equation: (((x-(-y))*2.4/(y)+((-y)+y)))
        let s = ((((x-(-y))*2.4/(y)+((-y)+y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((p-(-q))*2.4/(q)+((-q)+q)))
            z2 <== (((x-(-y))*2.4/(y)+((-y)+y)))
            wr.tt <| (I 712)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 713
        ctx.print.s "test713"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 713)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 714
        ctx.print.s "test714"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 714)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 715
        ctx.print.s "test715"
        //equation: ((-y)*3.0)
        let s = (((-y)*3.0)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)*3.0)
            z2 <== ((-y)*3.0)
            wr.tt <| (I 715)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 716
        ctx.print.s "test716"
        //equation: (((-5.5)+x*x*3.8*((-7.2)*(-8.6)*(-x))/(x))-4.3/(((-7.7)/2.8)+7.7/y+(-x))-(x+x+x)/(-x))
        let s = ((((-5.5)+x*x*3.8*((-7.2)*(-8.6)*(-x))/(x))-4.3/(((-7.7)/2.8)+7.7/y+(-x))-(x+x+x)/(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-5.5)+p*p*3.8*((-7.2)*(-8.6)*(-p))/(p))-4.3/(((-7.7)/2.8)+7.7/q+(-p))-(p+p+p)/(-p))
            z2 <== (((-5.5)+x*x*3.8*((-7.2)*(-8.6)*(-x))/(x))-4.3/(((-7.7)/2.8)+7.7/y+(-x))-(x+x+x)/(-x))
            wr.tt <| (I 716)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 717
        ctx.print.s "test717"
        //equation: 5.7
        //printfn "%d" 718
        ctx.print.s "test718"
        //equation: (6.2*3.8-(y+(-x)+(-1.6))/7.1-(-y)*(-3.1)+((2.8/(-6.5)*x-(-6.7)-(-0.1))))
        let s = ((6.2*3.8-(y+(-x)+(-1.6))/7.1-(-y)*(-3.1)+((2.8/(-6.5)*x-(-6.7)-(-0.1))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (6.2*3.8-(q+(-p)+(-1.6))/7.1-(-q)*(-3.1)+((2.8/(-6.5)*p-(-6.7)-(-0.1))))
            z2 <== (6.2*3.8-(y+(-x)+(-1.6))/7.1-(-y)*(-3.1)+((2.8/(-6.5)*x-(-6.7)-(-0.1))))
            wr.tt <| (I 718)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 719
        ctx.print.s "test719"
        //equation: (x*(-1.6)*1.6*(x/x/x+(-2.4)/3.1)-(-2.8)+y*(y+(-y))/((-y)-y-x+(4.1*y*6.5)+(-7.2)/x+(-7.6)*(-2.1)-(-y)))
        let s = ((x*(-1.6)*1.6*(x/x/x+(-2.4)/3.1)-(-2.8)+y*(y+(-y))/((-y)-y-x+(4.1*y*6.5)+(-7.2)/x+(-7.6)*(-2.1)-(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p*(-1.6)*1.6*(p/p/p+(-2.4)/3.1)-(-2.8)+q*(q+(-q))/((-q)-q-p+(4.1*q*6.5)+(-7.2)/p+(-7.6)*(-2.1)-(-q)))
            z2 <== (x*(-1.6)*1.6*(x/x/x+(-2.4)/3.1)-(-2.8)+y*(y+(-y))/((-y)-y-x+(4.1*y*6.5)+(-7.2)/x+(-7.6)*(-2.1)-(-y)))
            wr.tt <| (I 719)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 720
        ctx.print.s "test720"
        //equation: ((-y)*1.6-8.5/(8.1-(-y)+6.4))+8.5-y*y/y*(y/(6.4+(-x)/0.1)/((-0.4)))
        let s = (((-y)*1.6-8.5/(8.1-(-y)+6.4))+8.5-y*y/y*(y/(6.4+(-x)/0.1)/((-0.4)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)*1.6-8.5/(8.1-(-q)+6.4))+8.5-q*q/q*(q/(6.4+(-p)/0.1)/((-0.4)))
            z2 <== ((-y)*1.6-8.5/(8.1-(-y)+6.4))+8.5-y*y/y*(y/(6.4+(-x)/0.1)/((-0.4)))
            wr.tt <| (I 720)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 721
        ctx.print.s "test721"
        //equation: (3.2+((-1.2))-4.2-(-3.2)/x-x/y*y)*(-y)-1.6*(y)-x
        let s = ((3.2+((-1.2))-4.2-(-3.2)/x-x/y*y)*(-y)-1.6*(y)-x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (3.2+((-1.2))-4.2-(-3.2)/p-p/q*q)*(-q)-1.6*(q)-p
            z2 <== (3.2+((-1.2))-4.2-(-3.2)/x-x/y*y)*(-y)-1.6*(y)-x
            wr.tt <| (I 721)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 722
        ctx.print.s "test722"
        //equation: (-y)+(((-y))-0.7-x+y/y-(-6.1))
        let s = ((-y)+(((-y))-0.7-x+y/y-(-6.1))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)+(((-q))-0.7-p+q/q-(-6.1))
            z2 <== (-y)+(((-y))-0.7-x+y/y-(-6.1))
            wr.tt <| (I 722)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 723
        ctx.print.s "test723"
        //equation: (-2.1)
        //printfn "%d" 724
        ctx.print.s "test724"
        //equation: ((2.0*(1.7+y+(-2.7))*(-4.4)/x)/(8.2*(-y))+(((-y)/0.2+(-8.4)*(-2.4)+y))/(-4.7)*(-x))
        let s = (((2.0*(1.7+y+(-2.7))*(-4.4)/x)/(8.2*(-y))+(((-y)/0.2+(-8.4)*(-2.4)+y))/(-4.7)*(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((2.0*(1.7+q+(-2.7))*(-4.4)/p)/(8.2*(-q))+(((-q)/0.2+(-8.4)*(-2.4)+q))/(-4.7)*(-p))
            z2 <== ((2.0*(1.7+y+(-2.7))*(-4.4)/x)/(8.2*(-y))+(((-y)/0.2+(-8.4)*(-2.4)+y))/(-4.7)*(-x))
            wr.tt <| (I 724)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 725
        ctx.print.s "test725"
        //equation: ((((-x)*4.7/(-y)-(-4.3))+((-0.0)*y+y)+y*2.0)+((-x)-(-x)/(x*6.0-(-3.0))-3.1+(1.6-y/y)*(-6.6))+3.6-4.4-(-y))
        let s = (((((-x)*4.7/(-y)-(-4.3))+((-0.0)*y+y)+y*2.0)+((-x)-(-x)/(x*6.0-(-3.0))-3.1+(1.6-y/y)*(-6.6))+3.6-4.4-(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-p)*4.7/(-q)-(-4.3))+((-0.0)*q+q)+q*2.0)+((-p)-(-p)/(p*6.0-(-3.0))-3.1+(1.6-q/q)*(-6.6))+3.6-4.4-(-q))
            z2 <== ((((-x)*4.7/(-y)-(-4.3))+((-0.0)*y+y)+y*2.0)+((-x)-(-x)/(x*6.0-(-3.0))-3.1+(1.6-y/y)*(-6.6))+3.6-4.4-(-y))
            wr.tt <| (I 725)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 726
        ctx.print.s "test726"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 726)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 727
        ctx.print.s "test727"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 727)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 728
        ctx.print.s "test728"
        //equation: (((-4.8)+(-2.5)+1.4*((-7.1))/(x/6.7+(-x)))/5.6*(6.6-(-y)-(-0.3)/(-4.0)-(-0.1)))
        let s = ((((-4.8)+(-2.5)+1.4*((-7.1))/(x/6.7+(-x)))/5.6*(6.6-(-y)-(-0.3)/(-4.0)-(-0.1)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-4.8)+(-2.5)+1.4*((-7.1))/(p/6.7+(-p)))/5.6*(6.6-(-q)-(-0.3)/(-4.0)-(-0.1)))
            z2 <== (((-4.8)+(-2.5)+1.4*((-7.1))/(x/6.7+(-x)))/5.6*(6.6-(-y)-(-0.3)/(-4.0)-(-0.1)))
            wr.tt <| (I 728)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 729
        ctx.print.s "test729"
        //equation: (-1.0)
        //printfn "%d" 730
        ctx.print.s "test730"
        //equation: (-3.6)
        //printfn "%d" 731
        ctx.print.s "test731"
        //equation: ((-y))
        let s = (((-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q))
            z2 <== ((-y))
            wr.tt <| (I 731)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 732
        ctx.print.s "test732"
        //equation: (y+((-1.4)-(-y)+x+(-4.0)*((-1.5)-5.2))+(-y)-x)
        let s = ((y+((-1.4)-(-y)+x+(-4.0)*((-1.5)-5.2))+(-y)-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q+((-1.4)-(-q)+p+(-4.0)*((-1.5)-5.2))+(-q)-p)
            z2 <== (y+((-1.4)-(-y)+x+(-4.0)*((-1.5)-5.2))+(-y)-x)
            wr.tt <| (I 732)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 733
        ctx.print.s "test733"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 733)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 734
        ctx.print.s "test734"
        //equation: 3.4
        //printfn "%d" 735
        ctx.print.s "test735"
        //equation: (-7.2)
        //printfn "%d" 736
        ctx.print.s "test736"
        //equation: (((-y)*(-y)/(8.8)+((-1.6)*(-0.4)+y/y-(-y))*(x+x+(-5.0))))
        let s = ((((-y)*(-y)/(8.8)+((-1.6)*(-0.4)+y/y-(-y))*(x+x+(-5.0))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q)*(-q)/(8.8)+((-1.6)*(-0.4)+q/q-(-q))*(p+p+(-5.0))))
            z2 <== (((-y)*(-y)/(8.8)+((-1.6)*(-0.4)+y/y-(-y))*(x+x+(-5.0))))
            wr.tt <| (I 736)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 737
        ctx.print.s "test737"
        //equation: 6.7
        //printfn "%d" 738
        ctx.print.s "test738"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 738)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 739
        ctx.print.s "test739"
        //equation: (((-y)*y/(y)-y/y)*2.5/(y-1.2-5.1/(-5.6)/x/y+(-5.0))/(3.2))
        let s = ((((-y)*y/(y)-y/y)*2.5/(y-1.2-5.1/(-5.6)/x/y+(-5.0))/(3.2))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q)*q/(q)-q/q)*2.5/(q-1.2-5.1/(-5.6)/p/q+(-5.0))/(3.2))
            z2 <== (((-y)*y/(y)-y/y)*2.5/(y-1.2-5.1/(-5.6)/x/y+(-5.0))/(3.2))
            wr.tt <| (I 739)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 740
        ctx.print.s "test740"
        //equation: ((-y))
        let s = (((-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q))
            z2 <== ((-y))
            wr.tt <| (I 740)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 741
        ctx.print.s "test741"
        //equation: 7.4
        //printfn "%d" 742
        ctx.print.s "test742"
        //equation: (-0.7)/x*(2.4+y+(-2.6)-(-7.8)-y)
        let s = ((-0.7)/x*(2.4+y+(-2.6)-(-7.8)-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-0.7)/p*(2.4+q+(-2.6)-(-7.8)-q)
            z2 <== (-0.7)/x*(2.4+y+(-2.6)-(-7.8)-y)
            wr.tt <| (I 742)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 743
        ctx.print.s "test743"
        //equation: 7.0
        //printfn "%d" 744
        ctx.print.s "test744"
        //equation: ((((-6.6)+(-y))*(7.6*(-0.3)+x*(-7.2)*y)*y+(-2.8)+y+4.0-(-3.5))+(2.5)/0.4-(-7.5)-(-y)+(-4.6)*(-3.6)-(0.8-4.4+8.2*y*1.0)*(-y)/((-y)))
        let s = (((((-6.6)+(-y))*(7.6*(-0.3)+x*(-7.2)*y)*y+(-2.8)+y+4.0-(-3.5))+(2.5)/0.4-(-7.5)-(-y)+(-4.6)*(-3.6)-(0.8-4.4+8.2*y*1.0)*(-y)/((-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-6.6)+(-q))*(7.6*(-0.3)+p*(-7.2)*q)*q+(-2.8)+q+4.0-(-3.5))+(2.5)/0.4-(-7.5)-(-q)+(-4.6)*(-3.6)-(0.8-4.4+8.2*q*1.0)*(-q)/((-q)))
            z2 <== ((((-6.6)+(-y))*(7.6*(-0.3)+x*(-7.2)*y)*y+(-2.8)+y+4.0-(-3.5))+(2.5)/0.4-(-7.5)-(-y)+(-4.6)*(-3.6)-(0.8-4.4+8.2*y*1.0)*(-y)/((-y)))
            wr.tt <| (I 744)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 745
        ctx.print.s "test745"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 745)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 746
        ctx.print.s "test746"
        //equation: 0.8
        //printfn "%d" 747
        ctx.print.s "test747"
        //equation: 3.0
        //printfn "%d" 748
        ctx.print.s "test748"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 748)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 749
        ctx.print.s "test749"
        //equation: ((-6.1)-(y)+(x)+((4.8)*(-1.0)/((-y)/(-0.5)*(-y)+8.7-6.1)/y*2.1)-x)
        let s = (((-6.1)-(y)+(x)+((4.8)*(-1.0)/((-y)/(-0.5)*(-y)+8.7-6.1)/y*2.1)-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-6.1)-(q)+(p)+((4.8)*(-1.0)/((-q)/(-0.5)*(-q)+8.7-6.1)/q*2.1)-p)
            z2 <== ((-6.1)-(y)+(x)+((4.8)*(-1.0)/((-y)/(-0.5)*(-y)+8.7-6.1)/y*2.1)-x)
            wr.tt <| (I 749)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 750
        ctx.print.s "test750"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 750)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 751
        ctx.print.s "test751"
        //equation: ((y*(-2.2)+(-4.5)-(-3.8))*(-x))
        let s = (((y*(-2.2)+(-4.5)-(-3.8))*(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q*(-2.2)+(-4.5)-(-3.8))*(-p))
            z2 <== ((y*(-2.2)+(-4.5)-(-3.8))*(-x))
            wr.tt <| (I 751)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 752
        ctx.print.s "test752"
        //equation: (-4.3)
        //printfn "%d" 753
        ctx.print.s "test753"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 753)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 754
        ctx.print.s "test754"
        //equation: ((((-1.6)*6.6*1.8+(-6.2)/(-4.7))+((-y)-(-8.7)+(-1.1)+(-x))/(-y)-8.0)*((-7.7)*8.4/(-3.8)+(-3.6))-(2.6/x*(-x)*(-y)))
        let s = (((((-1.6)*6.6*1.8+(-6.2)/(-4.7))+((-y)-(-8.7)+(-1.1)+(-x))/(-y)-8.0)*((-7.7)*8.4/(-3.8)+(-3.6))-(2.6/x*(-x)*(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-1.6)*6.6*1.8+(-6.2)/(-4.7))+((-q)-(-8.7)+(-1.1)+(-p))/(-q)-8.0)*((-7.7)*8.4/(-3.8)+(-3.6))-(2.6/p*(-p)*(-q)))
            z2 <== ((((-1.6)*6.6*1.8+(-6.2)/(-4.7))+((-y)-(-8.7)+(-1.1)+(-x))/(-y)-8.0)*((-7.7)*8.4/(-3.8)+(-3.6))-(2.6/x*(-x)*(-y)))
            wr.tt <| (I 754)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 755
        ctx.print.s "test755"
        //equation: (-8.4)
        //printfn "%d" 756
        ctx.print.s "test756"
        //equation: (x-(((-x)/x/(-x)+x-(-3.8))-4.8/y/((-3.6)*3.5)-5.6)-(-8.7)-(-5.2)/((-y)/(-y)/8.7*(-y)-y)/x)
        let s = ((x-(((-x)/x/(-x)+x-(-3.8))-4.8/y/((-3.6)*3.5)-5.6)-(-8.7)-(-5.2)/((-y)/(-y)/8.7*(-y)-y)/x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p-(((-p)/p/(-p)+p-(-3.8))-4.8/q/((-3.6)*3.5)-5.6)-(-8.7)-(-5.2)/((-q)/(-q)/8.7*(-q)-q)/p)
            z2 <== (x-(((-x)/x/(-x)+x-(-3.8))-4.8/y/((-3.6)*3.5)-5.6)-(-8.7)-(-5.2)/((-y)/(-y)/8.7*(-y)-y)/x)
            wr.tt <| (I 756)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 757
        ctx.print.s "test757"
        //equation: 8.7
        //printfn "%d" 758
        ctx.print.s "test758"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 758)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 759
        ctx.print.s "test759"
        //equation: (((-8.7)-x-(y/(-y))+((-1.1)/x*y-(-x)*3.1)))
        let s = ((((-8.7)-x-(y/(-y))+((-1.1)/x*y-(-x)*3.1)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-8.7)-p-(q/(-q))+((-1.1)/p*q-(-p)*3.1)))
            z2 <== (((-8.7)-x-(y/(-y))+((-1.1)/x*y-(-x)*3.1)))
            wr.tt <| (I 759)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 760
        ctx.print.s "test760"
        //equation: (3.0/3.1*(2.5))
        //printfn "%d" 761
        ctx.print.s "test761"
        //equation: (y/(-y)+x+(-5.6)*1.3+y*(-y))
        let s = ((y/(-y)+x+(-5.6)*1.3+y*(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q/(-q)+p+(-5.6)*1.3+q*(-q))
            z2 <== (y/(-y)+x+(-5.6)*1.3+y*(-y))
            wr.tt <| (I 761)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 762
        ctx.print.s "test762"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 762)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 763
        ctx.print.s "test763"
        //equation: (6.6/1.5-(y-(6.8))*(8.6-x*0.1))
        let s = ((6.6/1.5-(y-(6.8))*(8.6-x*0.1))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (6.6/1.5-(q-(6.8))*(8.6-p*0.1))
            z2 <== (6.6/1.5-(y-(6.8))*(8.6-x*0.1))
            wr.tt <| (I 763)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 764
        ctx.print.s "test764"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 764)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 765
        ctx.print.s "test765"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 765)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 766
        ctx.print.s "test766"
        //equation: (y-(-2.8))
        let s = ((y-(-2.8))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q-(-2.8))
            z2 <== (y-(-2.8))
            wr.tt <| (I 766)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 767
        ctx.print.s "test767"
        //equation: (4.2)
        //printfn "%d" 768
        ctx.print.s "test768"
        //equation: 7.4
        //printfn "%d" 769
        ctx.print.s "test769"
        //equation: (0.0+((-x)/3.8/((-x)-x+(-4.6)-y))*(2.7+(-8.6)-y)*(y+(-3.2)-(-y)/(-7.2)+1.4)-x+(-2.6))
        let s = ((0.0+((-x)/3.8/((-x)-x+(-4.6)-y))*(2.7+(-8.6)-y)*(y+(-3.2)-(-y)/(-7.2)+1.4)-x+(-2.6))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (0.0+((-p)/3.8/((-p)-p+(-4.6)-q))*(2.7+(-8.6)-q)*(q+(-3.2)-(-q)/(-7.2)+1.4)-p+(-2.6))
            z2 <== (0.0+((-x)/3.8/((-x)-x+(-4.6)-y))*(2.7+(-8.6)-y)*(y+(-3.2)-(-y)/(-7.2)+1.4)-x+(-2.6))
            wr.tt <| (I 769)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 770
        ctx.print.s "test770"
        //equation: ((-y)/(-x)*(5.4+y-4.8*x)*(-y)*(-y)-((-x)*(-x)+(y/3.0)+2.7-y)/(((-y))/y))
        let s = (((-y)/(-x)*(5.4+y-4.8*x)*(-y)*(-y)-((-x)*(-x)+(y/3.0)+2.7-y)/(((-y))/y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)/(-p)*(5.4+q-4.8*p)*(-q)*(-q)-((-p)*(-p)+(q/3.0)+2.7-q)/(((-q))/q))
            z2 <== ((-y)/(-x)*(5.4+y-4.8*x)*(-y)*(-y)-((-x)*(-x)+(y/3.0)+2.7-y)/(((-y))/y))
            wr.tt <| (I 770)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 771
        ctx.print.s "test771"
        //equation: ((-7.4)*2.3/(-y)*y+x)
        let s = (((-7.4)*2.3/(-y)*y+x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-7.4)*2.3/(-q)*q+p)
            z2 <== ((-7.4)*2.3/(-y)*y+x)
            wr.tt <| (I 771)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 772
        ctx.print.s "test772"
        //equation: ((-y)-x-((-y)+5.0*x))
        let s = (((-y)-x-((-y)+5.0*x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q)-p-((-q)+5.0*p))
            z2 <== ((-y)-x-((-y)+5.0*x))
            wr.tt <| (I 772)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 773
        ctx.print.s "test773"
        //equation: (-x)-(2.5*(y-x*(-y)/y+(-y))*((-0.1)/y))+(-x)*(-4.1)+y
        let s = ((-x)-(2.5*(y-x*(-y)/y+(-y))*((-0.1)/y))+(-x)*(-4.1)+y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)-(2.5*(q-p*(-q)/q+(-q))*((-0.1)/q))+(-p)*(-4.1)+q
            z2 <== (-x)-(2.5*(y-x*(-y)/y+(-y))*((-0.1)/y))+(-x)*(-4.1)+y
            wr.tt <| (I 773)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 774
        ctx.print.s "test774"
        //equation: ((((-2.5)*3.5/(-x)+(-1.3))-(y-(-6.3)-7.0/y))/((y))+1.8/y)
        let s = (((((-2.5)*3.5/(-x)+(-1.3))-(y-(-6.3)-7.0/y))/((y))+1.8/y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-2.5)*3.5/(-p)+(-1.3))-(q-(-6.3)-7.0/q))/((q))+1.8/q)
            z2 <== ((((-2.5)*3.5/(-x)+(-1.3))-(y-(-6.3)-7.0/y))/((y))+1.8/y)
            wr.tt <| (I 774)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 775
        ctx.print.s "test775"
        //equation: 3.8
        //printfn "%d" 776
        ctx.print.s "test776"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 776)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 777
        ctx.print.s "test777"
        //equation: 3.5
        //printfn "%d" 778
        ctx.print.s "test778"
        //equation: (x)
        let s = ((x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p)
            z2 <== (x)
            wr.tt <| (I 778)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 779
        ctx.print.s "test779"
        //equation: (((-y))*((-y)+y+y*(-y))-(5.7/((-7.0)+(-y)+x-(-x))+y+(y+1.4/3.4/y))*((-y)*4.8+7.0/y)*(-y)-(-y)*(-4.1)*(-1.2)+((-x)-x*x-x)+(-8.1)-(3.4*(-x))+((-6.7)-(-y)))
        let s = ((((-y))*((-y)+y+y*(-y))-(5.7/((-7.0)+(-y)+x-(-x))+y+(y+1.4/3.4/y))*((-y)*4.8+7.0/y)*(-y)-(-y)*(-4.1)*(-1.2)+((-x)-x*x-x)+(-8.1)-(3.4*(-x))+((-6.7)-(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q))*((-q)+q+q*(-q))-(5.7/((-7.0)+(-q)+p-(-p))+q+(q+1.4/3.4/q))*((-q)*4.8+7.0/q)*(-q)-(-q)*(-4.1)*(-1.2)+((-p)-p*p-p)+(-8.1)-(3.4*(-p))+((-6.7)-(-q)))
            z2 <== (((-y))*((-y)+y+y*(-y))-(5.7/((-7.0)+(-y)+x-(-x))+y+(y+1.4/3.4/y))*((-y)*4.8+7.0/y)*(-y)-(-y)*(-4.1)*(-1.2)+((-x)-x*x-x)+(-8.1)-(3.4*(-x))+((-6.7)-(-y)))
            wr.tt <| (I 779)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 780
        ctx.print.s "test780"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 780)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 781
        ctx.print.s "test781"
        //equation: 3.4
        //printfn "%d" 782
        ctx.print.s "test782"
        //equation: (x/y+((x-(-y)-(-5.0)+3.5)*(-y)))
        let s = ((x/y+((x-(-y)-(-5.0)+3.5)*(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p/q+((p-(-q)-(-5.0)+3.5)*(-q)))
            z2 <== (x/y+((x-(-y)-(-5.0)+3.5)*(-y)))
            wr.tt <| (I 782)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 783
        ctx.print.s "test783"
        //equation: y-x*7.3+(3.1/(-3.7))/(-x)/(-y)/x-(-y)*(-4.1)/(y)*((-y)*7.2)/(x)
        let s = (y-x*7.3+(3.1/(-3.7))/(-x)/(-y)/x-(-y)*(-4.1)/(y)*((-y)*7.2)/(x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q-p*7.3+(3.1/(-3.7))/(-p)/(-q)/p-(-q)*(-4.1)/(q)*((-q)*7.2)/(p)
            z2 <== y-x*7.3+(3.1/(-3.7))/(-x)/(-y)/x-(-y)*(-4.1)/(y)*((-y)*7.2)/(x)
            wr.tt <| (I 783)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 784
        ctx.print.s "test784"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 784)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 785
        ctx.print.s "test785"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 785)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 786
        ctx.print.s "test786"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 786)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 787
        ctx.print.s "test787"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 787)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 788
        ctx.print.s "test788"
        //equation: ((-3.4)*(-x)-(-x)+((-y)/(-3.6)-6.0*(-x)-(-3.2)/(-1.2))*((y-(-y)/2.8-(-y))*((-x)*x/(-7.6)/(-1.7)-(-8.7))*x))
        let s = (((-3.4)*(-x)-(-x)+((-y)/(-3.6)-6.0*(-x)-(-3.2)/(-1.2))*((y-(-y)/2.8-(-y))*((-x)*x/(-7.6)/(-1.7)-(-8.7))*x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-3.4)*(-p)-(-p)+((-q)/(-3.6)-6.0*(-p)-(-3.2)/(-1.2))*((q-(-q)/2.8-(-q))*((-p)*p/(-7.6)/(-1.7)-(-8.7))*p))
            z2 <== ((-3.4)*(-x)-(-x)+((-y)/(-3.6)-6.0*(-x)-(-3.2)/(-1.2))*((y-(-y)/2.8-(-y))*((-x)*x/(-7.6)/(-1.7)-(-8.7))*x))
            wr.tt <| (I 788)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 789
        ctx.print.s "test789"
        //equation: ((x-y+6.1-(x)+(-x))/(-6.6)/(4.3-x)-(-1.2)-(7.3)/(-x)-0.7)
        let s = (((x-y+6.1-(x)+(-x))/(-6.6)/(4.3-x)-(-1.2)-(7.3)/(-x)-0.7)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p-q+6.1-(p)+(-p))/(-6.6)/(4.3-p)-(-1.2)-(7.3)/(-p)-0.7)
            z2 <== ((x-y+6.1-(x)+(-x))/(-6.6)/(4.3-x)-(-1.2)-(7.3)/(-x)-0.7)
            wr.tt <| (I 789)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 790
        ctx.print.s "test790"
        //equation: (6.6/(-y)+3.8-(-8.5)/6.2)+x
        let s = ((6.6/(-y)+3.8-(-8.5)/6.2)+x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (6.6/(-q)+3.8-(-8.5)/6.2)+p
            z2 <== (6.6/(-y)+3.8-(-8.5)/6.2)+x
            wr.tt <| (I 790)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 791
        ctx.print.s "test791"
        //equation: (-1.5)
        //printfn "%d" 792
        ctx.print.s "test792"
        //equation: (((x*x-(-1.6))))
        let s = ((((x*x-(-1.6))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((p*p-(-1.6))))
            z2 <== (((x*x-(-1.6))))
            wr.tt <| (I 792)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 793
        ctx.print.s "test793"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 793)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 794
        ctx.print.s "test794"
        //equation: (((8.7-1.4-(-x)*(-6.1)-(-y))-1.2+(-y))+(7.2-(-x)/(-x))+((y+1.5)-(y+y))*((6.3*(-x)/(-6.8))-7.6*((-y)-(-y))-((-4.4)*(-1.7)-(-y)*(-4.6)))-y)
        let s = ((((8.7-1.4-(-x)*(-6.1)-(-y))-1.2+(-y))+(7.2-(-x)/(-x))+((y+1.5)-(y+y))*((6.3*(-x)/(-6.8))-7.6*((-y)-(-y))-((-4.4)*(-1.7)-(-y)*(-4.6)))-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((8.7-1.4-(-p)*(-6.1)-(-q))-1.2+(-q))+(7.2-(-p)/(-p))+((q+1.5)-(q+q))*((6.3*(-p)/(-6.8))-7.6*((-q)-(-q))-((-4.4)*(-1.7)-(-q)*(-4.6)))-q)
            z2 <== (((8.7-1.4-(-x)*(-6.1)-(-y))-1.2+(-y))+(7.2-(-x)/(-x))+((y+1.5)-(y+y))*((6.3*(-x)/(-6.8))-7.6*((-y)-(-y))-((-4.4)*(-1.7)-(-y)*(-4.6)))-y)
            wr.tt <| (I 794)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 795
        ctx.print.s "test795"
        //equation: ((y+4.4-x*0.1*(-7.3)-(-3.8)*((-y)*0.2+(-7.1))+2.8)-((8.4+x)+(0.4)/(y-(-x)/5.0+1.6-(-1.1))+(2.2-(-4.0)))+(x+y/5.7+(1.6+(-x))/((-y)*x)))
        let s = (((y+4.4-x*0.1*(-7.3)-(-3.8)*((-y)*0.2+(-7.1))+2.8)-((8.4+x)+(0.4)/(y-(-x)/5.0+1.6-(-1.1))+(2.2-(-4.0)))+(x+y/5.7+(1.6+(-x))/((-y)*x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q+4.4-p*0.1*(-7.3)-(-3.8)*((-q)*0.2+(-7.1))+2.8)-((8.4+p)+(0.4)/(q-(-p)/5.0+1.6-(-1.1))+(2.2-(-4.0)))+(p+q/5.7+(1.6+(-p))/((-q)*p)))
            z2 <== ((y+4.4-x*0.1*(-7.3)-(-3.8)*((-y)*0.2+(-7.1))+2.8)-((8.4+x)+(0.4)/(y-(-x)/5.0+1.6-(-1.1))+(2.2-(-4.0)))+(x+y/5.7+(1.6+(-x))/((-y)*x)))
            wr.tt <| (I 795)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 796
        ctx.print.s "test796"
        //equation: 8.8+(((-3.3)+y)-3.1/x/(-y))
        let s = (8.8+(((-3.3)+y)-3.1/x/(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== 8.8+(((-3.3)+q)-3.1/p/(-q))
            z2 <== 8.8+(((-3.3)+y)-3.1/x/(-y))
            wr.tt <| (I 796)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 797
        ctx.print.s "test797"
        //equation: ((-x)+x/x-((-x)-(-x)*(-3.6))*8.0-(((-2.6)*(-y)*y)+((-y)/x)/1.8-8.3+(-8.6)/y/(-y)+((-y)*6.4-y*y+x))/((-2.1)/1.0*5.1-(-x)+7.7*x/((-y)))/((-x)*(-x)/(-y)))
        let s = (((-x)+x/x-((-x)-(-x)*(-3.6))*8.0-(((-2.6)*(-y)*y)+((-y)/x)/1.8-8.3+(-8.6)/y/(-y)+((-y)*6.4-y*y+x))/((-2.1)/1.0*5.1-(-x)+7.7*x/((-y)))/((-x)*(-x)/(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)+p/p-((-p)-(-p)*(-3.6))*8.0-(((-2.6)*(-q)*q)+((-q)/p)/1.8-8.3+(-8.6)/q/(-q)+((-q)*6.4-q*q+p))/((-2.1)/1.0*5.1-(-p)+7.7*p/((-q)))/((-p)*(-p)/(-q)))
            z2 <== ((-x)+x/x-((-x)-(-x)*(-3.6))*8.0-(((-2.6)*(-y)*y)+((-y)/x)/1.8-8.3+(-8.6)/y/(-y)+((-y)*6.4-y*y+x))/((-2.1)/1.0*5.1-(-x)+7.7*x/((-y)))/((-x)*(-x)/(-y)))
            wr.tt <| (I 797)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 798
        ctx.print.s "test798"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 798)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 799
        ctx.print.s "test799"
        //equation: (y+(-y)/(-2.2))
        let s = ((y+(-y)/(-2.2))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q+(-q)/(-2.2))
            z2 <== (y+(-y)/(-2.2))
            wr.tt <| (I 799)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 800
        ctx.print.s "test800"
        //equation: 8.7
        //printfn "%d" 801
        ctx.print.s "test801"
        //equation: 3.6
        //printfn "%d" 802
        ctx.print.s "test802"
        //equation: (-7.6)-(-6.5)-(-x)*0.6*(-y)/(-x)+5.4+(1.6-(-7.5))*((-8.2)-6.6-1.4/(-x)/(y+(-5.3)/(-7.5)+(-2.0)-(-x))*(-4.1))
        let s = ((-7.6)-(-6.5)-(-x)*0.6*(-y)/(-x)+5.4+(1.6-(-7.5))*((-8.2)-6.6-1.4/(-x)/(y+(-5.3)/(-7.5)+(-2.0)-(-x))*(-4.1))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-7.6)-(-6.5)-(-p)*0.6*(-q)/(-p)+5.4+(1.6-(-7.5))*((-8.2)-6.6-1.4/(-p)/(q+(-5.3)/(-7.5)+(-2.0)-(-p))*(-4.1))
            z2 <== (-7.6)-(-6.5)-(-x)*0.6*(-y)/(-x)+5.4+(1.6-(-7.5))*((-8.2)-6.6-1.4/(-x)/(y+(-5.3)/(-7.5)+(-2.0)-(-x))*(-4.1))
            wr.tt <| (I 802)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 803
        ctx.print.s "test803"
        //equation: ((8.4+(-x))/(y-(-1.2)*y+5.7)-(3.4*5.2)*5.6+5.3)*((-8.1))/x*(-y)
        let s = (((8.4+(-x))/(y-(-1.2)*y+5.7)-(3.4*5.2)*5.6+5.3)*((-8.1))/x*(-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((8.4+(-p))/(q-(-1.2)*q+5.7)-(3.4*5.2)*5.6+5.3)*((-8.1))/p*(-q)
            z2 <== ((8.4+(-x))/(y-(-1.2)*y+5.7)-(3.4*5.2)*5.6+5.3)*((-8.1))/x*(-y)
            wr.tt <| (I 803)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 804
        ctx.print.s "test804"
        //equation: ((-0.4)/(((-x)/(-y)*5.8)*(-7.7)-1.5/(-x)*(-x)-y)-8.7)
        let s = (((-0.4)/(((-x)/(-y)*5.8)*(-7.7)-1.5/(-x)*(-x)-y)-8.7)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-0.4)/(((-p)/(-q)*5.8)*(-7.7)-1.5/(-p)*(-p)-q)-8.7)
            z2 <== ((-0.4)/(((-x)/(-y)*5.8)*(-7.7)-1.5/(-x)*(-x)-y)-8.7)
            wr.tt <| (I 804)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 805
        ctx.print.s "test805"
        //equation: (((-y)+(-x)-(-3.2)*4.6)+(x/(-8.1))+y+y+x)/(-x)-(7.6/(-x)-6.4*(-x)-y)*(-x)*(((-6.4)*1.4-y*(-0.7))+((-x)))+(-3.6)*(y)
        let s = ((((-y)+(-x)-(-3.2)*4.6)+(x/(-8.1))+y+y+x)/(-x)-(7.6/(-x)-6.4*(-x)-y)*(-x)*(((-6.4)*1.4-y*(-0.7))+((-x)))+(-3.6)*(y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q)+(-p)-(-3.2)*4.6)+(p/(-8.1))+q+q+p)/(-p)-(7.6/(-p)-6.4*(-p)-q)*(-p)*(((-6.4)*1.4-q*(-0.7))+((-p)))+(-3.6)*(q)
            z2 <== (((-y)+(-x)-(-3.2)*4.6)+(x/(-8.1))+y+y+x)/(-x)-(7.6/(-x)-6.4*(-x)-y)*(-x)*(((-6.4)*1.4-y*(-0.7))+((-x)))+(-3.6)*(y)
            wr.tt <| (I 805)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 806
        ctx.print.s "test806"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 806)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 807
        ctx.print.s "test807"
        //equation: (y/2.2-(-y)-(-6.5)/x/(-8.4)-5.8-5.6/(-y)*((-x)+1.1)/(y-4.0/0.5+(-x)))
        let s = ((y/2.2-(-y)-(-6.5)/x/(-8.4)-5.8-5.6/(-y)*((-x)+1.1)/(y-4.0/0.5+(-x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q/2.2-(-q)-(-6.5)/p/(-8.4)-5.8-5.6/(-q)*((-p)+1.1)/(q-4.0/0.5+(-p)))
            z2 <== (y/2.2-(-y)-(-6.5)/x/(-8.4)-5.8-5.6/(-y)*((-x)+1.1)/(y-4.0/0.5+(-x)))
            wr.tt <| (I 807)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 808
        ctx.print.s "test808"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 808)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 809
        ctx.print.s "test809"
        //equation: (((-y))/((-4.2)*7.0-(-x)-((-y)-(-y))*x))
        let s = ((((-y))/((-4.2)*7.0-(-x)-((-y)-(-y))*x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q))/((-4.2)*7.0-(-p)-((-q)-(-q))*p))
            z2 <== (((-y))/((-4.2)*7.0-(-x)-((-y)-(-y))*x))
            wr.tt <| (I 809)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 810
        ctx.print.s "test810"
        //equation: (-7.4)
        //printfn "%d" 811
        ctx.print.s "test811"
        //equation: (y)
        let s = ((y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q)
            z2 <== (y)
            wr.tt <| (I 811)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 812
        ctx.print.s "test812"
        //equation: (4.5/4.3*(-y)+(-1.5)+((x+0.4-1.1)*(-8.6)))
        let s = ((4.5/4.3*(-y)+(-1.5)+((x+0.4-1.1)*(-8.6)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (4.5/4.3*(-q)+(-1.5)+((p+0.4-1.1)*(-8.6)))
            z2 <== (4.5/4.3*(-y)+(-1.5)+((x+0.4-1.1)*(-8.6)))
            wr.tt <| (I 812)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 813
        ctx.print.s "test813"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 813)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 814
        ctx.print.s "test814"
        //equation: ((-8.8))
        //printfn "%d" 815
        ctx.print.s "test815"
        //equation: ((-y))
        let s = (((-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q))
            z2 <== ((-y))
            wr.tt <| (I 815)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 816
        ctx.print.s "test816"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 816)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 817
        ctx.print.s "test817"
        //equation: ((-2.3)/(-x)+(-8.4))
        let s = (((-2.3)/(-x)+(-8.4))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-2.3)/(-p)+(-8.4))
            z2 <== ((-2.3)/(-x)+(-8.4))
            wr.tt <| (I 817)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 818
        ctx.print.s "test818"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 818)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 819
        ctx.print.s "test819"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 819)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 820
        ctx.print.s "test820"
        //equation: (y*(-7.6)-3.1)
        let s = ((y*(-7.6)-3.1)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q*(-7.6)-3.1)
            z2 <== (y*(-7.6)-3.1)
            wr.tt <| (I 820)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 821
        ctx.print.s "test821"
        //equation: (8.5)
        //printfn "%d" 822
        ctx.print.s "test822"
        //equation: (2.4)+(-2.2)-8.7+x
        let s = ((2.4)+(-2.2)-8.7+x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (2.4)+(-2.2)-8.7+p
            z2 <== (2.4)+(-2.2)-8.7+x
            wr.tt <| (I 822)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 823
        ctx.print.s "test823"
        //equation: ((2.7)+(-1.5))
        //printfn "%d" 824
        ctx.print.s "test824"
        //equation: (((2.5-y*6.6)+0.0*((-x)-3.3-y*(-y))))
        let s = ((((2.5-y*6.6)+0.0*((-x)-3.3-y*(-y))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((2.5-q*6.6)+0.0*((-p)-3.3-q*(-q))))
            z2 <== (((2.5-y*6.6)+0.0*((-x)-3.3-y*(-y))))
            wr.tt <| (I 824)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 825
        ctx.print.s "test825"
        //equation: (y*(7.1)*(3.2*(6.4)-(-x)+(-y))-(x))
        let s = ((y*(7.1)*(3.2*(6.4)-(-x)+(-y))-(x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q*(7.1)*(3.2*(6.4)-(-p)+(-q))-(p))
            z2 <== (y*(7.1)*(3.2*(6.4)-(-x)+(-y))-(x))
            wr.tt <| (I 825)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 826
        ctx.print.s "test826"
        //equation: 7.2
        //printfn "%d" 827
        ctx.print.s "test827"
        //equation: (x*(3.0)-(y-(-x)*(-1.6))+(-x)/(-4.0)/y/(-x)-1.4-(-y)/y)/(-1.6)-(((-x)/2.6*1.4)+((-x))+(8.2*(-x)-y*7.4-6.8)+6.4)/(((-x))-((-x)-0.3/1.8*x)/(x-(-y)/y+(-x)))/6.7
        let s = ((x*(3.0)-(y-(-x)*(-1.6))+(-x)/(-4.0)/y/(-x)-1.4-(-y)/y)/(-1.6)-(((-x)/2.6*1.4)+((-x))+(8.2*(-x)-y*7.4-6.8)+6.4)/(((-x))-((-x)-0.3/1.8*x)/(x-(-y)/y+(-x)))/6.7).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p*(3.0)-(q-(-p)*(-1.6))+(-p)/(-4.0)/q/(-p)-1.4-(-q)/q)/(-1.6)-(((-p)/2.6*1.4)+((-p))+(8.2*(-p)-q*7.4-6.8)+6.4)/(((-p))-((-p)-0.3/1.8*p)/(p-(-q)/q+(-p)))/6.7
            z2 <== (x*(3.0)-(y-(-x)*(-1.6))+(-x)/(-4.0)/y/(-x)-1.4-(-y)/y)/(-1.6)-(((-x)/2.6*1.4)+((-x))+(8.2*(-x)-y*7.4-6.8)+6.4)/(((-x))-((-x)-0.3/1.8*x)/(x-(-y)/y+(-x)))/6.7
            wr.tt <| (I 827)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 828
        ctx.print.s "test828"
        //equation: (-5.5)
        //printfn "%d" 829
        ctx.print.s "test829"
        //equation: (-4.5)
        //printfn "%d" 830
        ctx.print.s "test830"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 830)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 831
        ctx.print.s "test831"
        //equation: (((y/(-3.3))*(-6.7)-(8.6*y+(-6.1)*y/(-y))-x/(-7.5))-(-7.7)/y+(x/(-1.6)*(-7.3)/(-y)*(-x)))
        let s = ((((y/(-3.3))*(-6.7)-(8.6*y+(-6.1)*y/(-y))-x/(-7.5))-(-7.7)/y+(x/(-1.6)*(-7.3)/(-y)*(-x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((q/(-3.3))*(-6.7)-(8.6*q+(-6.1)*q/(-q))-p/(-7.5))-(-7.7)/q+(p/(-1.6)*(-7.3)/(-q)*(-p)))
            z2 <== (((y/(-3.3))*(-6.7)-(8.6*y+(-6.1)*y/(-y))-x/(-7.5))-(-7.7)/y+(x/(-1.6)*(-7.3)/(-y)*(-x)))
            wr.tt <| (I 831)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 832
        ctx.print.s "test832"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 832)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 833
        ctx.print.s "test833"
        //equation: ((-5.4)-8.4/0.3/((-0.1)/0.1))
        //printfn "%d" 834
        ctx.print.s "test834"
        //equation: (-3.8)
        //printfn "%d" 835
        ctx.print.s "test835"
        //equation: (-5.1)+(((-y)+(-3.4))+(-1.5)*((-y)+(-5.7)/x)*(-8.6))
        let s = ((-5.1)+(((-y)+(-3.4))+(-1.5)*((-y)+(-5.7)/x)*(-8.6))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-5.1)+(((-q)+(-3.4))+(-1.5)*((-q)+(-5.7)/p)*(-8.6))
            z2 <== (-5.1)+(((-y)+(-3.4))+(-1.5)*((-y)+(-5.7)/x)*(-8.6))
            wr.tt <| (I 835)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 836
        ctx.print.s "test836"
        //equation: (y-(-8.3)/4.8*(-y)-(-y))
        let s = ((y-(-8.3)/4.8*(-y)-(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q-(-8.3)/4.8*(-q)-(-q))
            z2 <== (y-(-8.3)/4.8*(-y)-(-y))
            wr.tt <| (I 836)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 837
        ctx.print.s "test837"
        //equation: ((-6.0)*y+(((-7.5)-6.3/x)+y+(-3.4)-y)-((-4.4)-5.0)/(6.3-(-5.4)*(-2.6)/((-y)-5.2/(-y)/x/6.7)+((-y)+(-y)+(-x)*5.4-6.6)))
        let s = (((-6.0)*y+(((-7.5)-6.3/x)+y+(-3.4)-y)-((-4.4)-5.0)/(6.3-(-5.4)*(-2.6)/((-y)-5.2/(-y)/x/6.7)+((-y)+(-y)+(-x)*5.4-6.6)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-6.0)*q+(((-7.5)-6.3/p)+q+(-3.4)-q)-((-4.4)-5.0)/(6.3-(-5.4)*(-2.6)/((-q)-5.2/(-q)/p/6.7)+((-q)+(-q)+(-p)*5.4-6.6)))
            z2 <== ((-6.0)*y+(((-7.5)-6.3/x)+y+(-3.4)-y)-((-4.4)-5.0)/(6.3-(-5.4)*(-2.6)/((-y)-5.2/(-y)/x/6.7)+((-y)+(-y)+(-x)*5.4-6.6)))
            wr.tt <| (I 837)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 838
        ctx.print.s "test838"
        //equation: (1.1+(-y)-x*1.2/(-4.4)+7.1+y*8.7-(0.5-(y*4.8/(-6.4)+x/4.5))-x+(-6.6)+x-y*(-y))
        let s = ((1.1+(-y)-x*1.2/(-4.4)+7.1+y*8.7-(0.5-(y*4.8/(-6.4)+x/4.5))-x+(-6.6)+x-y*(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (1.1+(-q)-p*1.2/(-4.4)+7.1+q*8.7-(0.5-(q*4.8/(-6.4)+p/4.5))-p+(-6.6)+p-q*(-q))
            z2 <== (1.1+(-y)-x*1.2/(-4.4)+7.1+y*8.7-(0.5-(y*4.8/(-6.4)+x/4.5))-x+(-6.6)+x-y*(-y))
            wr.tt <| (I 838)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 839
        ctx.print.s "test839"
        //equation: (((-6.6))-(y+2.2/(-3.0))+(-6.2)-(((-y)-(-y)-2.5/(-y)+y))/(-5.0)/(-x)*(-5.8)*(8.6*y*6.5/6.7)+((-5.1)+(-0.7)+(-3.3)+(-y)*1.7))
        let s = ((((-6.6))-(y+2.2/(-3.0))+(-6.2)-(((-y)-(-y)-2.5/(-y)+y))/(-5.0)/(-x)*(-5.8)*(8.6*y*6.5/6.7)+((-5.1)+(-0.7)+(-3.3)+(-y)*1.7))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-6.6))-(q+2.2/(-3.0))+(-6.2)-(((-q)-(-q)-2.5/(-q)+q))/(-5.0)/(-p)*(-5.8)*(8.6*q*6.5/6.7)+((-5.1)+(-0.7)+(-3.3)+(-q)*1.7))
            z2 <== (((-6.6))-(y+2.2/(-3.0))+(-6.2)-(((-y)-(-y)-2.5/(-y)+y))/(-5.0)/(-x)*(-5.8)*(8.6*y*6.5/6.7)+((-5.1)+(-0.7)+(-3.3)+(-y)*1.7))
            wr.tt <| (I 839)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 840
        ctx.print.s "test840"
        //equation: ((-7.7)+x)
        let s = (((-7.7)+x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-7.7)+p)
            z2 <== ((-7.7)+x)
            wr.tt <| (I 840)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 841
        ctx.print.s "test841"
        //equation: 2.2
        //printfn "%d" 842
        ctx.print.s "test842"
        //equation: ((-7.4)/4.3+(-y)*((-6.2)))
        let s = (((-7.4)/4.3+(-y)*((-6.2)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-7.4)/4.3+(-q)*((-6.2)))
            z2 <== ((-7.4)/4.3+(-y)*((-6.2)))
            wr.tt <| (I 842)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 843
        ctx.print.s "test843"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 843)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 844
        ctx.print.s "test844"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 844)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 845
        ctx.print.s "test845"
        //equation: (y/y)
        let s = ((y/y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q/q)
            z2 <== (y/y)
            wr.tt <| (I 845)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 846
        ctx.print.s "test846"
        //equation: ((-5.2)+(-x))
        let s = (((-5.2)+(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-5.2)+(-p))
            z2 <== ((-5.2)+(-x))
            wr.tt <| (I 846)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 847
        ctx.print.s "test847"
        //equation: (y)
        let s = ((y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q)
            z2 <== (y)
            wr.tt <| (I 847)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 848
        ctx.print.s "test848"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 848)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 849
        ctx.print.s "test849"
        //equation: (((-4.1)*(-7.5))*(0.4+y/(-0.6)/3.4-1.2/(-7.4))-1.2-y)
        let s = ((((-4.1)*(-7.5))*(0.4+y/(-0.6)/3.4-1.2/(-7.4))-1.2-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-4.1)*(-7.5))*(0.4+q/(-0.6)/3.4-1.2/(-7.4))-1.2-q)
            z2 <== (((-4.1)*(-7.5))*(0.4+y/(-0.6)/3.4-1.2/(-7.4))-1.2-y)
            wr.tt <| (I 849)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 850
        ctx.print.s "test850"
        //equation: ((6.2/(-y)-(-8.5)-(-y)/0.8*0.5*(y+3.8))-(-0.3)+x-(-x))
        let s = (((6.2/(-y)-(-8.5)-(-y)/0.8*0.5*(y+3.8))-(-0.3)+x-(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((6.2/(-q)-(-8.5)-(-q)/0.8*0.5*(q+3.8))-(-0.3)+p-(-p))
            z2 <== ((6.2/(-y)-(-8.5)-(-y)/0.8*0.5*(y+3.8))-(-0.3)+x-(-x))
            wr.tt <| (I 850)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 851
        ctx.print.s "test851"
        //equation: (x-(-1.3)+(((-3.7)+x*4.7*(-x)/(-y))*(x/6.5))/(8.6-y-(-x)))
        let s = ((x-(-1.3)+(((-3.7)+x*4.7*(-x)/(-y))*(x/6.5))/(8.6-y-(-x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p-(-1.3)+(((-3.7)+p*4.7*(-p)/(-q))*(p/6.5))/(8.6-q-(-p)))
            z2 <== (x-(-1.3)+(((-3.7)+x*4.7*(-x)/(-y))*(x/6.5))/(8.6-y-(-x)))
            wr.tt <| (I 851)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 852
        ctx.print.s "test852"
        //equation: ((y-(-x)-(-5.6))*(-y)*(-3.8))
        let s = (((y-(-x)-(-5.6))*(-y)*(-3.8))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q-(-p)-(-5.6))*(-q)*(-3.8))
            z2 <== ((y-(-x)-(-5.6))*(-y)*(-3.8))
            wr.tt <| (I 852)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 853
        ctx.print.s "test853"
        //equation: (-1.8)
        //printfn "%d" 854
        ctx.print.s "test854"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 854)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 855
        ctx.print.s "test855"
        //equation: ((x)+(-1.1))
        let s = (((x)+(-1.1))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p)+(-1.1))
            z2 <== ((x)+(-1.1))
            wr.tt <| (I 855)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 856
        ctx.print.s "test856"
        //equation: (-4.5)
        //printfn "%d" 857
        ctx.print.s "test857"
        //equation: (-5.4)
        //printfn "%d" 858
        ctx.print.s "test858"
        //equation: (((-5.3)*7.2-(-6.1))/8.5/y+0.0/y+((-x)*5.3)-2.3-(-5.8)+2.3*1.6)+(-x)
        let s = ((((-5.3)*7.2-(-6.1))/8.5/y+0.0/y+((-x)*5.3)-2.3-(-5.8)+2.3*1.6)+(-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-5.3)*7.2-(-6.1))/8.5/q+0.0/q+((-p)*5.3)-2.3-(-5.8)+2.3*1.6)+(-p)
            z2 <== (((-5.3)*7.2-(-6.1))/8.5/y+0.0/y+((-x)*5.3)-2.3-(-5.8)+2.3*1.6)+(-x)
            wr.tt <| (I 858)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 859
        ctx.print.s "test859"
        //equation: 5.0
        //printfn "%d" 860
        ctx.print.s "test860"
        //equation: (-7.1)*(((-3.5)))+(-8.4)*(-5.2)/(-y)
        let s = ((-7.1)*(((-3.5)))+(-8.4)*(-5.2)/(-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-7.1)*(((-3.5)))+(-8.4)*(-5.2)/(-q)
            z2 <== (-7.1)*(((-3.5)))+(-8.4)*(-5.2)/(-y)
            wr.tt <| (I 860)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 861
        ctx.print.s "test861"
        //equation: ((1.0*((-x)+(-3.0)+(-x)*3.3)+(-y)-(8.3*(-y)-y/(-x)))/y+((-y)+(-2.6)+((-5.2))))
        let s = (((1.0*((-x)+(-3.0)+(-x)*3.3)+(-y)-(8.3*(-y)-y/(-x)))/y+((-y)+(-2.6)+((-5.2))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((1.0*((-p)+(-3.0)+(-p)*3.3)+(-q)-(8.3*(-q)-q/(-p)))/q+((-q)+(-2.6)+((-5.2))))
            z2 <== ((1.0*((-x)+(-3.0)+(-x)*3.3)+(-y)-(8.3*(-y)-y/(-x)))/y+((-y)+(-2.6)+((-5.2))))
            wr.tt <| (I 861)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 862
        ctx.print.s "test862"
        //equation: (0.6+(-4.3)*7.6/y-(-4.3)+(-x)-(-4.8)-y)*1.1/x
        let s = ((0.6+(-4.3)*7.6/y-(-4.3)+(-x)-(-4.8)-y)*1.1/x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (0.6+(-4.3)*7.6/q-(-4.3)+(-p)-(-4.8)-q)*1.1/p
            z2 <== (0.6+(-4.3)*7.6/y-(-4.3)+(-x)-(-4.8)-y)*1.1/x
            wr.tt <| (I 862)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 863
        ctx.print.s "test863"
        //equation: (x*((-0.4)/(-y)-(-x)+(-y))-((-x)+(-2.2)*4.8*y)*8.3+((-3.2))+(-x)-(x+3.8*y-(-y)+(-4.3))/(-x)/(-0.2)/((-y)*2.8*(-5.6)/(-5.8)-(-7.5))-((-3.2)))
        let s = ((x*((-0.4)/(-y)-(-x)+(-y))-((-x)+(-2.2)*4.8*y)*8.3+((-3.2))+(-x)-(x+3.8*y-(-y)+(-4.3))/(-x)/(-0.2)/((-y)*2.8*(-5.6)/(-5.8)-(-7.5))-((-3.2)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p*((-0.4)/(-q)-(-p)+(-q))-((-p)+(-2.2)*4.8*q)*8.3+((-3.2))+(-p)-(p+3.8*q-(-q)+(-4.3))/(-p)/(-0.2)/((-q)*2.8*(-5.6)/(-5.8)-(-7.5))-((-3.2)))
            z2 <== (x*((-0.4)/(-y)-(-x)+(-y))-((-x)+(-2.2)*4.8*y)*8.3+((-3.2))+(-x)-(x+3.8*y-(-y)+(-4.3))/(-x)/(-0.2)/((-y)*2.8*(-5.6)/(-5.8)-(-7.5))-((-3.2)))
            wr.tt <| (I 863)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 864
        ctx.print.s "test864"
        //equation: 1.2
        //printfn "%d" 865
        ctx.print.s "test865"
        //equation: ((-y))-((-y)-(-y)+y-(2.6))*(-x)/(6.7*(-y)/y+3.2/(-x)+x+(x+(-x)*(-x)))
        let s = (((-y))-((-y)-(-y)+y-(2.6))*(-x)/(6.7*(-y)/y+3.2/(-x)+x+(x+(-x)*(-x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q))-((-q)-(-q)+q-(2.6))*(-p)/(6.7*(-q)/q+3.2/(-p)+p+(p+(-p)*(-p)))
            z2 <== ((-y))-((-y)-(-y)+y-(2.6))*(-x)/(6.7*(-y)/y+3.2/(-x)+x+(x+(-x)*(-x)))
            wr.tt <| (I 865)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 866
        ctx.print.s "test866"
        //equation: (-5.5)
        //printfn "%d" 867
        ctx.print.s "test867"
        //equation: (((-y)+(-y)-(-y)))
        let s = ((((-y)+(-y)-(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q)+(-q)-(-q)))
            z2 <== (((-y)+(-y)-(-y)))
            wr.tt <| (I 867)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 868
        ctx.print.s "test868"
        //equation: (-5.4)
        //printfn "%d" 869
        ctx.print.s "test869"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 869)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 870
        ctx.print.s "test870"
        //equation: (1.8-(-3.6)*(-y)-(-x)+(x*(-y)-5.2)/(-y))
        let s = ((1.8-(-3.6)*(-y)-(-x)+(x*(-y)-5.2)/(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (1.8-(-3.6)*(-q)-(-p)+(p*(-q)-5.2)/(-q))
            z2 <== (1.8-(-3.6)*(-y)-(-x)+(x*(-y)-5.2)/(-y))
            wr.tt <| (I 870)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 871
        ctx.print.s "test871"
        //equation: (-4.0)+(-8.3)
        //printfn "%d" 872
        ctx.print.s "test872"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 872)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 873
        ctx.print.s "test873"
        //equation: ((-1.8)-(3.8/(-y)-(6.3-(-2.6)/(-y)/(-1.6))*x-y-(-y)))
        let s = (((-1.8)-(3.8/(-y)-(6.3-(-2.6)/(-y)/(-1.6))*x-y-(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-1.8)-(3.8/(-q)-(6.3-(-2.6)/(-q)/(-1.6))*p-q-(-q)))
            z2 <== ((-1.8)-(3.8/(-y)-(6.3-(-2.6)/(-y)/(-1.6))*x-y-(-y)))
            wr.tt <| (I 873)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 874
        ctx.print.s "test874"
        //equation: ((((-8.8)+(-5.3)-(-8.3)*(-4.2)/(-y))+(-x))*(0.5+x*(-5.7)-x-7.5*(5.7+x+(-4.8)-(-1.7))/(3.5/y)))
        let s = (((((-8.8)+(-5.3)-(-8.3)*(-4.2)/(-y))+(-x))*(0.5+x*(-5.7)-x-7.5*(5.7+x+(-4.8)-(-1.7))/(3.5/y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-8.8)+(-5.3)-(-8.3)*(-4.2)/(-q))+(-p))*(0.5+p*(-5.7)-p-7.5*(5.7+p+(-4.8)-(-1.7))/(3.5/q)))
            z2 <== ((((-8.8)+(-5.3)-(-8.3)*(-4.2)/(-y))+(-x))*(0.5+x*(-5.7)-x-7.5*(5.7+x+(-4.8)-(-1.7))/(3.5/y)))
            wr.tt <| (I 874)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 875
        ctx.print.s "test875"
        //equation: (x/5.2/x+6.6)
        let s = ((x/5.2/x+6.6)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p/5.2/p+6.6)
            z2 <== (x/5.2/x+6.6)
            wr.tt <| (I 875)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 876
        ctx.print.s "test876"
        //equation: (-4.7)
        //printfn "%d" 877
        ctx.print.s "test877"
        //equation: 6.3
        //printfn "%d" 878
        ctx.print.s "test878"
        //equation: 7.1
        //printfn "%d" 879
        ctx.print.s "test879"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 879)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 880
        ctx.print.s "test880"
        //equation: 8.7
        //printfn "%d" 881
        ctx.print.s "test881"
        //equation: ((-y))
        let s = (((-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-q))
            z2 <== ((-y))
            wr.tt <| (I 881)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 882
        ctx.print.s "test882"
        //equation: y+(-y)
        let s = (y+(-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q+(-q)
            z2 <== y+(-y)
            wr.tt <| (I 882)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 883
        ctx.print.s "test883"
        //equation: (((-2.2))+(y-5.5-(-5.4)*3.3+(-y)))
        let s = ((((-2.2))+(y-5.5-(-5.4)*3.3+(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-2.2))+(q-5.5-(-5.4)*3.3+(-q)))
            z2 <== (((-2.2))+(y-5.5-(-5.4)*3.3+(-y)))
            wr.tt <| (I 883)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 884
        ctx.print.s "test884"
        //equation: (((-4.4)+(-0.0)*(0.7+(-5.3)+(-4.1)/x+0.6)))
        let s = ((((-4.4)+(-0.0)*(0.7+(-5.3)+(-4.1)/x+0.6)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-4.4)+(-0.0)*(0.7+(-5.3)+(-4.1)/p+0.6)))
            z2 <== (((-4.4)+(-0.0)*(0.7+(-5.3)+(-4.1)/x+0.6)))
            wr.tt <| (I 884)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 885
        ctx.print.s "test885"
        //equation: (((-1.6))-(-x)*(4.6-(-y)*1.5/(-1.7)+(-x))*(((-0.5)+(-6.4)*8.0+(-7.6)*y)*((-y))+(y+y+(-x)-(-y)/(-x))))
        let s = ((((-1.6))-(-x)*(4.6-(-y)*1.5/(-1.7)+(-x))*(((-0.5)+(-6.4)*8.0+(-7.6)*y)*((-y))+(y+y+(-x)-(-y)/(-x))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-1.6))-(-p)*(4.6-(-q)*1.5/(-1.7)+(-p))*(((-0.5)+(-6.4)*8.0+(-7.6)*q)*((-q))+(q+q+(-p)-(-q)/(-p))))
            z2 <== (((-1.6))-(-x)*(4.6-(-y)*1.5/(-1.7)+(-x))*(((-0.5)+(-6.4)*8.0+(-7.6)*y)*((-y))+(y+y+(-x)-(-y)/(-x))))
            wr.tt <| (I 885)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 886
        ctx.print.s "test886"
        //equation: (1.3-((-1.3))-x+y-(-6.0)*(-x))*y/(((-x)+x/8.5/(-x)))*x-2.0
        let s = ((1.3-((-1.3))-x+y-(-6.0)*(-x))*y/(((-x)+x/8.5/(-x)))*x-2.0).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (1.3-((-1.3))-p+q-(-6.0)*(-p))*q/(((-p)+p/8.5/(-p)))*p-2.0
            z2 <== (1.3-((-1.3))-x+y-(-6.0)*(-x))*y/(((-x)+x/8.5/(-x)))*x-2.0
            wr.tt <| (I 886)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 887
        ctx.print.s "test887"
        //equation: (((-y)/(-y))*(((-x)*(-x)-(-x)-(-x)+(-7.3))-x-(-y)+y-x*(-x))/(y/(x+0.0/3.3)+(4.1+y/(-6.1)-(-y)-(-y))*(-6.4))*(((-x))-y+5.7-(-2.8)/(-y))-6.2*(y+6.6)-(-y)-((-0.1)*y))
        let s = ((((-y)/(-y))*(((-x)*(-x)-(-x)-(-x)+(-7.3))-x-(-y)+y-x*(-x))/(y/(x+0.0/3.3)+(4.1+y/(-6.1)-(-y)-(-y))*(-6.4))*(((-x))-y+5.7-(-2.8)/(-y))-6.2*(y+6.6)-(-y)-((-0.1)*y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q)/(-q))*(((-p)*(-p)-(-p)-(-p)+(-7.3))-p-(-q)+q-p*(-p))/(q/(p+0.0/3.3)+(4.1+q/(-6.1)-(-q)-(-q))*(-6.4))*(((-p))-q+5.7-(-2.8)/(-q))-6.2*(q+6.6)-(-q)-((-0.1)*q))
            z2 <== (((-y)/(-y))*(((-x)*(-x)-(-x)-(-x)+(-7.3))-x-(-y)+y-x*(-x))/(y/(x+0.0/3.3)+(4.1+y/(-6.1)-(-y)-(-y))*(-6.4))*(((-x))-y+5.7-(-2.8)/(-y))-6.2*(y+6.6)-(-y)-((-0.1)*y))
            wr.tt <| (I 887)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 888
        ctx.print.s "test888"
        //equation: 2.3
        //printfn "%d" 889
        ctx.print.s "test889"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 889)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 890
        ctx.print.s "test890"
        //equation: (-3.5)
        //printfn "%d" 891
        ctx.print.s "test891"
        //equation: (((-x)+y/(-5.1)/(-8.5)-(-y)*y-((-y)*0.1))/(-y))
        let s = ((((-x)+y/(-5.1)/(-8.5)-(-y)*y-((-y)*0.1))/(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-p)+q/(-5.1)/(-8.5)-(-q)*q-((-q)*0.1))/(-q))
            z2 <== (((-x)+y/(-5.1)/(-8.5)-(-y)*y-((-y)*0.1))/(-y))
            wr.tt <| (I 891)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 892
        ctx.print.s "test892"
        //equation: (((-6.4)-x+(-x)*(1.1*(-y)-2.6/3.2/1.5))+(-y)/3.1)
        let s = ((((-6.4)-x+(-x)*(1.1*(-y)-2.6/3.2/1.5))+(-y)/3.1)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-6.4)-p+(-p)*(1.1*(-q)-2.6/3.2/1.5))+(-q)/3.1)
            z2 <== (((-6.4)-x+(-x)*(1.1*(-y)-2.6/3.2/1.5))+(-y)/3.1)
            wr.tt <| (I 892)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 893
        ctx.print.s "test893"
        //equation: (y+2.1+x)
        let s = ((y+2.1+x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q+2.1+p)
            z2 <== (y+2.1+x)
            wr.tt <| (I 893)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 894
        ctx.print.s "test894"
        //equation: ((-2.7)/0.0-0.1-y)+y+(-2.4)*1.5+(-y)-(-y)
        let s = (((-2.7)/0.0-0.1-y)+y+(-2.4)*1.5+(-y)-(-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-2.7)/0.0-0.1-q)+q+(-2.4)*1.5+(-q)-(-q)
            z2 <== ((-2.7)/0.0-0.1-y)+y+(-2.4)*1.5+(-y)-(-y)
            wr.tt <| (I 894)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 895
        ctx.print.s "test895"
        //equation: (-6.1)+(-y)+(-0.5)
        let s = ((-6.1)+(-y)+(-0.5)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-6.1)+(-q)+(-0.5)
            z2 <== (-6.1)+(-y)+(-0.5)
            wr.tt <| (I 895)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 896
        ctx.print.s "test896"
        //equation: ((((-x)/(-y)/(-1.8))/x+2.5+(-1.4)+(-x)*y/((-4.4)*(-y)+(-y)-(-y)*y)/(-3.6)))
        let s = (((((-x)/(-y)/(-1.8))/x+2.5+(-1.4)+(-x)*y/((-4.4)*(-y)+(-y)-(-y)*y)/(-3.6)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-p)/(-q)/(-1.8))/p+2.5+(-1.4)+(-p)*q/((-4.4)*(-q)+(-q)-(-q)*q)/(-3.6)))
            z2 <== ((((-x)/(-y)/(-1.8))/x+2.5+(-1.4)+(-x)*y/((-4.4)*(-y)+(-y)-(-y)*y)/(-3.6)))
            wr.tt <| (I 896)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 897
        ctx.print.s "test897"
        //equation: (-7.2)
        //printfn "%d" 898
        ctx.print.s "test898"
        //equation: 3.3
        //printfn "%d" 899
        ctx.print.s "test899"
        //equation: ((5.6))
        //printfn "%d" 900
        ctx.print.s "test900"
        //equation: (((-x)-y-(8.4+0.6-(-x)))*(((-0.7)-4.0)*(-4.2))/(0.0-(-6.6)/y/y)/(-7.8)/((-1.7)*x/(-6.7)*8.4/(-0.8)-4.0-(-0.5)))
        let s = ((((-x)-y-(8.4+0.6-(-x)))*(((-0.7)-4.0)*(-4.2))/(0.0-(-6.6)/y/y)/(-7.8)/((-1.7)*x/(-6.7)*8.4/(-0.8)-4.0-(-0.5)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-p)-q-(8.4+0.6-(-p)))*(((-0.7)-4.0)*(-4.2))/(0.0-(-6.6)/q/q)/(-7.8)/((-1.7)*p/(-6.7)*8.4/(-0.8)-4.0-(-0.5)))
            z2 <== (((-x)-y-(8.4+0.6-(-x)))*(((-0.7)-4.0)*(-4.2))/(0.0-(-6.6)/y/y)/(-7.8)/((-1.7)*x/(-6.7)*8.4/(-0.8)-4.0-(-0.5)))
            wr.tt <| (I 900)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 901
        ctx.print.s "test901"
        //equation: x-(-2.0)
        let s = (x-(-2.0)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p-(-2.0)
            z2 <== x-(-2.0)
            wr.tt <| (I 901)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 902
        ctx.print.s "test902"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 902)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 903
        ctx.print.s "test903"
        //equation: (-6.6)
        //printfn "%d" 904
        ctx.print.s "test904"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 904)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 905
        ctx.print.s "test905"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 905)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 906
        ctx.print.s "test906"
        //equation: (-8.4)
        //printfn "%d" 907
        ctx.print.s "test907"
        //equation: 4.1*(-y)/(-x)/(x+(-2.3)*x*7.7)/(5.4+(-y)-1.1+(-6.5)/y)/x+x+3.2/(-1.0)
        let s = (4.1*(-y)/(-x)/(x+(-2.3)*x*7.7)/(5.4+(-y)-1.1+(-6.5)/y)/x+x+3.2/(-1.0)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== 4.1*(-q)/(-p)/(p+(-2.3)*p*7.7)/(5.4+(-q)-1.1+(-6.5)/q)/p+p+3.2/(-1.0)
            z2 <== 4.1*(-y)/(-x)/(x+(-2.3)*x*7.7)/(5.4+(-y)-1.1+(-6.5)/y)/x+x+3.2/(-1.0)
            wr.tt <| (I 907)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 908
        ctx.print.s "test908"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 908)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 909
        ctx.print.s "test909"
        //equation: (x-(((-y)/0.1*x)*(-6.2)))
        let s = ((x-(((-y)/0.1*x)*(-6.2)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p-(((-q)/0.1*p)*(-6.2)))
            z2 <== (x-(((-y)/0.1*x)*(-6.2)))
            wr.tt <| (I 909)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 910
        ctx.print.s "test910"
        //equation: (-x)+x*(-3.5)/y/(-3.1)+y/y-(((-x)))/(-y)-((x)/(7.7/8.7/4.4))
        let s = ((-x)+x*(-3.5)/y/(-3.1)+y/y-(((-x)))/(-y)-((x)/(7.7/8.7/4.4))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)+p*(-3.5)/q/(-3.1)+q/q-(((-p)))/(-q)-((p)/(7.7/8.7/4.4))
            z2 <== (-x)+x*(-3.5)/y/(-3.1)+y/y-(((-x)))/(-y)-((x)/(7.7/8.7/4.4))
            wr.tt <| (I 910)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 911
        ctx.print.s "test911"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 911)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 912
        ctx.print.s "test912"
        //equation: (-4.1)
        //printfn "%d" 913
        ctx.print.s "test913"
        //equation: ((-0.6)+(-y)*(-y)+(-0.2))
        let s = (((-0.6)+(-y)*(-y)+(-0.2))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-0.6)+(-q)*(-q)+(-0.2))
            z2 <== ((-0.6)+(-y)*(-y)+(-0.2))
            wr.tt <| (I 913)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 914
        ctx.print.s "test914"
        //equation: ((-4.5)-1.3+(y-((-1.4)/x/x-(-y)*(-x))*(-5.8)-(1.5*1.8-(-x)-4.6))+(x-(-x)+x*(-x)))
        let s = (((-4.5)-1.3+(y-((-1.4)/x/x-(-y)*(-x))*(-5.8)-(1.5*1.8-(-x)-4.6))+(x-(-x)+x*(-x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-4.5)-1.3+(q-((-1.4)/p/p-(-q)*(-p))*(-5.8)-(1.5*1.8-(-p)-4.6))+(p-(-p)+p*(-p)))
            z2 <== ((-4.5)-1.3+(y-((-1.4)/x/x-(-y)*(-x))*(-5.8)-(1.5*1.8-(-x)-4.6))+(x-(-x)+x*(-x)))
            wr.tt <| (I 914)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 915
        ctx.print.s "test915"
        //equation: (x/(y+7.3-(-y))/1.7+x/8.4/(x+(-6.6)-((-4.5)-y-y*(-8.2))))
        let s = ((x/(y+7.3-(-y))/1.7+x/8.4/(x+(-6.6)-((-4.5)-y-y*(-8.2))))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p/(q+7.3-(-q))/1.7+p/8.4/(p+(-6.6)-((-4.5)-q-q*(-8.2))))
            z2 <== (x/(y+7.3-(-y))/1.7+x/8.4/(x+(-6.6)-((-4.5)-y-y*(-8.2))))
            wr.tt <| (I 915)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 916
        ctx.print.s "test916"
        //equation: ((3.1/(-x)*(-x)/(-2.8)/(-1.5))/x)-(-0.3)*(((-3.3)-7.2*(-3.5))-y-(8.0/(-0.5)/y))
        let s = (((3.1/(-x)*(-x)/(-2.8)/(-1.5))/x)-(-0.3)*(((-3.3)-7.2*(-3.5))-y-(8.0/(-0.5)/y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((3.1/(-p)*(-p)/(-2.8)/(-1.5))/p)-(-0.3)*(((-3.3)-7.2*(-3.5))-q-(8.0/(-0.5)/q))
            z2 <== ((3.1/(-x)*(-x)/(-2.8)/(-1.5))/x)-(-0.3)*(((-3.3)-7.2*(-3.5))-y-(8.0/(-0.5)/y))
            wr.tt <| (I 916)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 917
        ctx.print.s "test917"
        //equation: ((1.5-(-2.0)/(-y))*(-y)-((-x)))
        let s = (((1.5-(-2.0)/(-y))*(-y)-((-x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((1.5-(-2.0)/(-q))*(-q)-((-p)))
            z2 <== ((1.5-(-2.0)/(-y))*(-y)-((-x)))
            wr.tt <| (I 917)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 918
        ctx.print.s "test918"
        //equation: (((-y)-y/y+x/(-3.2)*(-x)*x))
        let s = ((((-y)-y/y+x/(-3.2)*(-x)*x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q)-q/q+p/(-3.2)*(-p)*p))
            z2 <== (((-y)-y/y+x/(-3.2)*(-x)*x))
            wr.tt <| (I 918)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 919
        ctx.print.s "test919"
        //equation: 2.8
        //printfn "%d" 920
        ctx.print.s "test920"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 920)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 921
        ctx.print.s "test921"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 921)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 922
        ctx.print.s "test922"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 922)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 923
        ctx.print.s "test923"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 923)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 924
        ctx.print.s "test924"
        //equation: (-1.0)
        //printfn "%d" 925
        ctx.print.s "test925"
        //equation: 5.1
        //printfn "%d" 926
        ctx.print.s "test926"
        //equation: (x+(-8.3)-(-0.3)/7.3)+x*(7.7)
        let s = ((x+(-8.3)-(-0.3)/7.3)+x*(7.7)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (p+(-8.3)-(-0.3)/7.3)+p*(7.7)
            z2 <== (x+(-8.3)-(-0.3)/7.3)+x*(7.7)
            wr.tt <| (I 926)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 927
        ctx.print.s "test927"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 927)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 928
        ctx.print.s "test928"
        //equation: (-0.7)
        //printfn "%d" 929
        ctx.print.s "test929"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 929)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 930
        ctx.print.s "test930"
        //equation: (y+(x/(y-(-7.0))-1.0-(y*4.3))-7.1)
        let s = ((y+(x/(y-(-7.0))-1.0-(y*4.3))-7.1)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q+(p/(q-(-7.0))-1.0-(q*4.3))-7.1)
            z2 <== (y+(x/(y-(-7.0))-1.0-(y*4.3))-7.1)
            wr.tt <| (I 930)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 931
        ctx.print.s "test931"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 931)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 932
        ctx.print.s "test932"
        //equation: ((-2.7))
        //printfn "%d" 933
        ctx.print.s "test933"
        //equation: (-4.6)
        //printfn "%d" 934
        ctx.print.s "test934"
        //equation: ((((-3.4)+x-4.8+x)-(-x)/(3.3/3.5-(-y)/1.3)+(-y))-((-x)/((-5.7)-(-1.3)*x-(-2.3)*6.8)*(5.4+x-(-5.5)+(-y)))+(-x))
        let s = (((((-3.4)+x-4.8+x)-(-x)/(3.3/3.5-(-y)/1.3)+(-y))-((-x)/((-5.7)-(-1.3)*x-(-2.3)*6.8)*(5.4+x-(-5.5)+(-y)))+(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-3.4)+p-4.8+p)-(-p)/(3.3/3.5-(-q)/1.3)+(-q))-((-p)/((-5.7)-(-1.3)*p-(-2.3)*6.8)*(5.4+p-(-5.5)+(-q)))+(-p))
            z2 <== ((((-3.4)+x-4.8+x)-(-x)/(3.3/3.5-(-y)/1.3)+(-y))-((-x)/((-5.7)-(-1.3)*x-(-2.3)*6.8)*(5.4+x-(-5.5)+(-y)))+(-x))
            wr.tt <| (I 934)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 935
        ctx.print.s "test935"
        //equation: (((-y)+(-7.1)*(-1.6))/8.8)
        let s = ((((-y)+(-7.1)*(-1.6))/8.8)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q)+(-7.1)*(-1.6))/8.8)
            z2 <== (((-y)+(-7.1)*(-1.6))/8.8)
            wr.tt <| (I 935)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 936
        ctx.print.s "test936"
        //equation: (-7.7)
        //printfn "%d" 937
        ctx.print.s "test937"
        //equation: (-1.8)
        //printfn "%d" 938
        ctx.print.s "test938"
        //equation: 0.8
        //printfn "%d" 939
        ctx.print.s "test939"
        //equation: (y)
        let s = ((y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q)
            z2 <== (y)
            wr.tt <| (I 939)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 940
        ctx.print.s "test940"
        //equation: (-1.8)
        //printfn "%d" 941
        ctx.print.s "test941"
        //equation: (2.4)
        //printfn "%d" 942
        ctx.print.s "test942"
        //equation: ((-0.7)+(-1.6)-2.7+(-y)-x)
        let s = (((-0.7)+(-1.6)-2.7+(-y)-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-0.7)+(-1.6)-2.7+(-q)-p)
            z2 <== ((-0.7)+(-1.6)-2.7+(-y)-x)
            wr.tt <| (I 942)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 943
        ctx.print.s "test943"
        //equation: (y+(-7.8)*((y/y/(-y)+y+3.1)-x*((-x))*(x-x))-(-x)-(-x))
        let s = ((y+(-7.8)*((y/y/(-y)+y+3.1)-x*((-x))*(x-x))-(-x)-(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q+(-7.8)*((q/q/(-q)+q+3.1)-p*((-p))*(p-p))-(-p)-(-p))
            z2 <== (y+(-7.8)*((y/y/(-y)+y+3.1)-x*((-x))*(x-x))-(-x)-(-x))
            wr.tt <| (I 943)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 944
        ctx.print.s "test944"
        //equation: (((-y)/(-x)+((-y)))/(-6.3)-((y+2.2-4.8+6.4*(-2.2))*y))
        let s = ((((-y)/(-x)+((-y)))/(-6.3)-((y+2.2-4.8+6.4*(-2.2))*y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q)/(-p)+((-q)))/(-6.3)-((q+2.2-4.8+6.4*(-2.2))*q))
            z2 <== (((-y)/(-x)+((-y)))/(-6.3)-((y+2.2-4.8+6.4*(-2.2))*y))
            wr.tt <| (I 944)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 945
        ctx.print.s "test945"
        //equation: 4.8-((2.2-(-y)-3.4/4.4+y)+y-y/((-2.3)-(-y)/8.1-(-2.2)))+((-y)+x)
        let s = (4.8-((2.2-(-y)-3.4/4.4+y)+y-y/((-2.3)-(-y)/8.1-(-2.2)))+((-y)+x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== 4.8-((2.2-(-q)-3.4/4.4+q)+q-q/((-2.3)-(-q)/8.1-(-2.2)))+((-q)+p)
            z2 <== 4.8-((2.2-(-y)-3.4/4.4+y)+y-y/((-2.3)-(-y)/8.1-(-2.2)))+((-y)+x)
            wr.tt <| (I 945)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 946
        ctx.print.s "test946"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 946)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 947
        ctx.print.s "test947"
        //equation: ((-4.8)/((y-(-y)/(-x)*(-1.7)/(-x))-(-x)+6.5)-1.1/(((-x)*(-y)/1.7*(-2.2))*4.2+1.1/y+y+y-x*3.5))
        let s = (((-4.8)/((y-(-y)/(-x)*(-1.7)/(-x))-(-x)+6.5)-1.1/(((-x)*(-y)/1.7*(-2.2))*4.2+1.1/y+y+y-x*3.5))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-4.8)/((q-(-q)/(-p)*(-1.7)/(-p))-(-p)+6.5)-1.1/(((-p)*(-q)/1.7*(-2.2))*4.2+1.1/q+q+q-p*3.5))
            z2 <== ((-4.8)/((y-(-y)/(-x)*(-1.7)/(-x))-(-x)+6.5)-1.1/(((-x)*(-y)/1.7*(-2.2))*4.2+1.1/y+y+y-x*3.5))
            wr.tt <| (I 947)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 948
        ctx.print.s "test948"
        //equation: (-7.1)
        //printfn "%d" 949
        ctx.print.s "test949"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 949)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 950
        ctx.print.s "test950"
        //equation: (-7.1)
        //printfn "%d" 951
        ctx.print.s "test951"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 951)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 952
        ctx.print.s "test952"
        //equation: 2.7
        //printfn "%d" 953
        ctx.print.s "test953"
        //equation: (-5.8)
        //printfn "%d" 954
        ctx.print.s "test954"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 954)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 955
        ctx.print.s "test955"
        //equation: (-1.1)
        //printfn "%d" 956
        ctx.print.s "test956"
        //equation: (-x)/3.6
        let s = ((-x)/3.6).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)/3.6
            z2 <== (-x)/3.6
            wr.tt <| (I 956)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 957
        ctx.print.s "test957"
        //equation: (0.1+(-x))
        let s = ((0.1+(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (0.1+(-p))
            z2 <== (0.1+(-x))
            wr.tt <| (I 957)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 958
        ctx.print.s "test958"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 958)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 959
        ctx.print.s "test959"
        //equation: ((((-y)-x)+((-y)/(-0.2)/(-4.7))-(-x)+y))
        let s = (((((-y)-x)+((-y)/(-0.2)/(-4.7))-(-x)+y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((((-q)-p)+((-q)/(-0.2)/(-4.7))-(-p)+q))
            z2 <== ((((-y)-x)+((-y)/(-0.2)/(-4.7))-(-x)+y))
            wr.tt <| (I 959)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 960
        ctx.print.s "test960"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 960)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 961
        ctx.print.s "test961"
        //equation: 7.2
        //printfn "%d" 962
        ctx.print.s "test962"
        //equation: 4.5
        //printfn "%d" 963
        ctx.print.s "test963"
        //equation: (y-y)
        let s = ((y-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (q-q)
            z2 <== (y-y)
            wr.tt <| (I 963)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 964
        ctx.print.s "test964"
        //equation: ((0.1+(-8.2)/(-4.6)+y+8.2))/(y*(-4.5))-(-2.4)/(y)-(x)+x
        let s = (((0.1+(-8.2)/(-4.6)+y+8.2))/(y*(-4.5))-(-2.4)/(y)-(x)+x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((0.1+(-8.2)/(-4.6)+q+8.2))/(q*(-4.5))-(-2.4)/(q)-(p)+p
            z2 <== ((0.1+(-8.2)/(-4.6)+y+8.2))/(y*(-4.5))-(-2.4)/(y)-(x)+x
            wr.tt <| (I 964)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 965
        ctx.print.s "test965"
        //equation: (-4.1)
        //printfn "%d" 966
        ctx.print.s "test966"
        //equation: (((-x)/(4.8/(-x))+x/(-x))*(-5.2)-(((-y)-(-5.3))*3.7-x*(y*y*y)*x))
        let s = ((((-x)/(4.8/(-x))+x/(-x))*(-5.2)-(((-y)-(-5.3))*3.7-x*(y*y*y)*x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-p)/(4.8/(-p))+p/(-p))*(-5.2)-(((-q)-(-5.3))*3.7-p*(q*q*q)*p))
            z2 <== (((-x)/(4.8/(-x))+x/(-x))*(-5.2)-(((-y)-(-5.3))*3.7-x*(y*y*y)*x))
            wr.tt <| (I 966)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 967
        ctx.print.s "test967"
        //equation: (-2.0)
        //printfn "%d" 968
        ctx.print.s "test968"
        //equation: (-5.4)-x+((y*y*(-x)+(-5.8))*((-x)+(-6.3)-6.1))/((-5.1)/(-0.5)+4.1/(-x))/((-y)*1.5/((-2.6)/2.6*y))
        let s = ((-5.4)-x+((y*y*(-x)+(-5.8))*((-x)+(-6.3)-6.1))/((-5.1)/(-0.5)+4.1/(-x))/((-y)*1.5/((-2.6)/2.6*y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-5.4)-p+((q*q*(-p)+(-5.8))*((-p)+(-6.3)-6.1))/((-5.1)/(-0.5)+4.1/(-p))/((-q)*1.5/((-2.6)/2.6*q))
            z2 <== (-5.4)-x+((y*y*(-x)+(-5.8))*((-x)+(-6.3)-6.1))/((-5.1)/(-0.5)+4.1/(-x))/((-y)*1.5/((-2.6)/2.6*y))
            wr.tt <| (I 968)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 969
        ctx.print.s "test969"
        //equation: (5.3-(-2.8)/(-4.4)/((-2.8)-(3.4*x)*(6.4))+2.3)
        let s = ((5.3-(-2.8)/(-4.4)/((-2.8)-(3.4*x)*(6.4))+2.3)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (5.3-(-2.8)/(-4.4)/((-2.8)-(3.4*p)*(6.4))+2.3)
            z2 <== (5.3-(-2.8)/(-4.4)/((-2.8)-(3.4*x)*(6.4))+2.3)
            wr.tt <| (I 969)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 970
        ctx.print.s "test970"
        //equation: ((-8.2)/(x/8.2-(-x)-(-x)+(-y)*y/y/(-x))*((x)/(y*(-y)/y-(-5.1))+8.7/((-y)-x)/7.8)-(((-y)/8.4*y)-(-0.8)*((-x)-(-5.4)-8.0-(-8.7)))*(-2.6))
        let s = (((-8.2)/(x/8.2-(-x)-(-x)+(-y)*y/y/(-x))*((x)/(y*(-y)/y-(-5.1))+8.7/((-y)-x)/7.8)-(((-y)/8.4*y)-(-0.8)*((-x)-(-5.4)-8.0-(-8.7)))*(-2.6))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-8.2)/(p/8.2-(-p)-(-p)+(-q)*q/q/(-p))*((p)/(q*(-q)/q-(-5.1))+8.7/((-q)-p)/7.8)-(((-q)/8.4*q)-(-0.8)*((-p)-(-5.4)-8.0-(-8.7)))*(-2.6))
            z2 <== ((-8.2)/(x/8.2-(-x)-(-x)+(-y)*y/y/(-x))*((x)/(y*(-y)/y-(-5.1))+8.7/((-y)-x)/7.8)-(((-y)/8.4*y)-(-0.8)*((-x)-(-5.4)-8.0-(-8.7)))*(-2.6))
            wr.tt <| (I 970)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 971
        ctx.print.s "test971"
        //equation: (((-y)+(1.8*0.5))-(-y)*4.6-y/(x+(1.2-(-x)-(-2.6)-(-y))+(1.5)*(-y)))
        let s = ((((-y)+(1.8*0.5))-(-y)*4.6-y/(x+(1.2-(-x)-(-2.6)-(-y))+(1.5)*(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-q)+(1.8*0.5))-(-q)*4.6-q/(p+(1.2-(-p)-(-2.6)-(-q))+(1.5)*(-q)))
            z2 <== (((-y)+(1.8*0.5))-(-y)*4.6-y/(x+(1.2-(-x)-(-2.6)-(-y))+(1.5)*(-y)))
            wr.tt <| (I 971)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 972
        ctx.print.s "test972"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 972)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 973
        ctx.print.s "test973"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 973)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 974
        ctx.print.s "test974"
        //equation: 2.3+(8.1+y-5.5/(-7.6)*(x/x)/((-1.6)/y*x/y))
        let s = (2.3+(8.1+y-5.5/(-7.6)*(x/x)/((-1.6)/y*x/y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== 2.3+(8.1+q-5.5/(-7.6)*(p/p)/((-1.6)/q*p/q))
            z2 <== 2.3+(8.1+y-5.5/(-7.6)*(x/x)/((-1.6)/y*x/y))
            wr.tt <| (I 974)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 975
        ctx.print.s "test975"
        //equation: ((-1.2)/(4.3)/(-x))
        let s = (((-1.2)/(4.3)/(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-1.2)/(4.3)/(-p))
            z2 <== ((-1.2)/(4.3)/(-x))
            wr.tt <| (I 975)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 976
        ctx.print.s "test976"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 976)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 977
        ctx.print.s "test977"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 977)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 978
        ctx.print.s "test978"
        //equation: 0.4
        //printfn "%d" 979
        ctx.print.s "test979"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 979)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 980
        ctx.print.s "test980"
        //equation: ((x*y-1.3*(-6.2))*(-7.5)/(-6.7))
        let s = (((x*y-1.3*(-6.2))*(-7.5)/(-6.7))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p*q-1.3*(-6.2))*(-7.5)/(-6.7))
            z2 <== ((x*y-1.3*(-6.2))*(-7.5)/(-6.7))
            wr.tt <| (I 980)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 981
        ctx.print.s "test981"
        //equation: (-2.7)
        //printfn "%d" 982
        ctx.print.s "test982"
        //equation: (-y)
        let s = ((-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 982)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 983
        ctx.print.s "test983"
        //equation: 0.1
        //printfn "%d" 984
        ctx.print.s "test984"
        //equation: x
        let s = (x).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p
            z2 <== x
            wr.tt <| (I 984)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 985
        ctx.print.s "test985"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 985)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 986
        ctx.print.s "test986"
        //equation: (((-5.0)/(-x)+x/3.3*(-y))*(0.1+(-6.2))-((-5.7)*(-y))*(-x))
        let s = ((((-5.0)/(-x)+x/3.3*(-y))*(0.1+(-6.2))-((-5.7)*(-y))*(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-5.0)/(-p)+p/3.3*(-q))*(0.1+(-6.2))-((-5.7)*(-q))*(-p))
            z2 <== (((-5.0)/(-x)+x/3.3*(-y))*(0.1+(-6.2))-((-5.7)*(-y))*(-x))
            wr.tt <| (I 986)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 987
        ctx.print.s "test987"
        //equation: ((-7.3)+((-1.0)/y/(-y)+(-x))-((-y)-8.2)*((-x)*(-y)-(-y))+6.8*((y/6.2)+(-3.7)+(x*x-7.6+(-5.3))*x/(-5.0)*(-x)-(-0.6)+(-4.4))+((5.3)/(y-y)-(-y)))
        let s = (((-7.3)+((-1.0)/y/(-y)+(-x))-((-y)-8.2)*((-x)*(-y)-(-y))+6.8*((y/6.2)+(-3.7)+(x*x-7.6+(-5.3))*x/(-5.0)*(-x)-(-0.6)+(-4.4))+((5.3)/(y-y)-(-y)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-7.3)+((-1.0)/q/(-q)+(-p))-((-q)-8.2)*((-p)*(-q)-(-q))+6.8*((q/6.2)+(-3.7)+(p*p-7.6+(-5.3))*p/(-5.0)*(-p)-(-0.6)+(-4.4))+((5.3)/(q-q)-(-q)))
            z2 <== ((-7.3)+((-1.0)/y/(-y)+(-x))-((-y)-8.2)*((-x)*(-y)-(-y))+6.8*((y/6.2)+(-3.7)+(x*x-7.6+(-5.3))*x/(-5.0)*(-x)-(-0.6)+(-4.4))+((5.3)/(y-y)-(-y)))
            wr.tt <| (I 987)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 988
        ctx.print.s "test988"
        //equation: ((-x)*(-y)*x+y+x-0.7)
        let s = (((-x)*(-y)*x+y+x-0.7)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((-p)*(-q)*p+q+p-0.7)
            z2 <== ((-x)*(-y)*x+y+x-0.7)
            wr.tt <| (I 988)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 989
        ctx.print.s "test989"
        //equation: 8.1
        //printfn "%d" 990
        ctx.print.s "test990"
        //equation: ((y*((-6.2)-(-6.8)+y)/y-x+6.5*7.3-(-y)+(y-(-x)+(-4.7)/2.5)+(x))-((-y)+(-x)-(0.0*(-1.1))/(6.3-y))+y)
        let s = (((y*((-6.2)-(-6.8)+y)/y-x+6.5*7.3-(-y)+(y-(-x)+(-4.7)/2.5)+(x))-((-y)+(-x)-(0.0*(-1.1))/(6.3-y))+y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((q*((-6.2)-(-6.8)+q)/q-p+6.5*7.3-(-q)+(q-(-p)+(-4.7)/2.5)+(p))-((-q)+(-p)-(0.0*(-1.1))/(6.3-q))+q)
            z2 <== ((y*((-6.2)-(-6.8)+y)/y-x+6.5*7.3-(-y)+(y-(-x)+(-4.7)/2.5)+(x))-((-y)+(-x)-(0.0*(-1.1))/(6.3-y))+y)
            wr.tt <| (I 990)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 991
        ctx.print.s "test991"
        //equation: ((6.1*(1.8-x*(-x)))/(-x))
        let s = (((6.1*(1.8-x*(-x)))/(-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((6.1*(1.8-p*(-p)))/(-p))
            z2 <== ((6.1*(1.8-x*(-x)))/(-x))
            wr.tt <| (I 991)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 992
        ctx.print.s "test992"
        //equation: 4.2
        //printfn "%d" 993
        ctx.print.s "test993"
        //equation: 2.4
        //printfn "%d" 994
        ctx.print.s "test994"
        //equation: (((-5.3)*y+(-x)+y)*0.5+x/((y+x)))
        let s = ((((-5.3)*y+(-x)+y)*0.5+x/((y+x)))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (((-5.3)*q+(-p)+q)*0.5+p/((q+p)))
            z2 <== (((-5.3)*y+(-x)+y)*0.5+x/((y+x)))
            wr.tt <| (I 994)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 995
        ctx.print.s "test995"
        //equation: ((-5.8))
        //printfn "%d" 996
        ctx.print.s "test996"
        //equation: (-x)
        let s = ((-x)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 996)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 997
        ctx.print.s "test997"
        //equation: ((x/(-7.2)+(x-y*(-x)+3.1/8.0)-x))
        let s = (((x/(-7.2)+(x-y*(-x)+3.1/8.0)-x))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== ((p/(-7.2)+(p-q*(-p)+3.1/8.0)-p))
            z2 <== ((x/(-7.2)+(x-y*(-x)+3.1/8.0)-x))
            wr.tt <| (I 997)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 998
        ctx.print.s "test998"
        //equation: (-x)/5.1+(-y)/(x+(-x)*(-2.2)+5.8+8.5)/0.3-((-x)*5.1)/((-y)-x+3.0-y)
        let s = ((-x)/5.1+(-y)/(x+(-x)*(-2.2)+5.8+8.5)/0.3-((-x)*5.1)/((-y)-x+3.0-y)).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== (-p)/5.1+(-q)/(p+(-p)*(-2.2)+5.8+8.5)/0.3-((-p)*5.1)/((-q)-p+3.0-q)
            z2 <== (-x)/5.1+(-y)/(x+(-x)*(-2.2)+5.8+8.5)/0.3-((-x)*5.1)/((-y)-x+3.0-y)
            wr.tt <| (I 998)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 999
        ctx.print.s "test999"
        //equation: x/((1.1-5.6+0.1*y)*7.3)/(y)*(-x)*((-8.6)*0.3)+(y-(-x)/(-3.8)+y)/y+(-x)/(-y)*2.2+6.3+(((-x)/y*(-5.8))*(y/(-y)*y)*(-x)+8.7+(-y))
        let s = (x/((1.1-5.6+0.1*y)*7.3)/(y)*(-x)*((-8.6)*0.3)+(y-(-x)/(-3.8)+y)/y+(-x)/(-y)*2.2+6.3+(((-x)/y*(-5.8))*(y/(-y)*y)*(-x)+8.7+(-y))).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== p/((1.1-5.6+0.1*q)*7.3)/(q)*(-p)*((-8.6)*0.3)+(q-(-p)/(-3.8)+q)/q+(-p)/(-q)*2.2+6.3+(((-p)/q*(-5.8))*(q/(-q)*q)*(-p)+8.7+(-q))
            z2 <== x/((1.1-5.6+0.1*y)*7.3)/(y)*(-x)*((-8.6)*0.3)+(y-(-x)/(-3.8)+y)/y+(-x)/(-y)*2.2+6.3+(((-x)/y*(-5.8))*(y/(-y)*y)*(-x)+8.7+(-y))
            wr.tt <| (I 999)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 1000
        ctx.print.s "test1000"
        //equation: y
        let s = (y).Expr.simp.eval()
        if s.ToString().Contains("NaN") then
            ctx.print.s "NaN"
        elif s.ToString().Contains("∞") then
            ctx.print.s "Infinity"
        else
            z1 <== q
            z2 <== y
            wr.tt <| (I 1000)++z1++z2++asm.abs(z1-z2)
