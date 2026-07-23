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
    ctx.ch.dddd <| fun (x,y,z1,z2) ->
        let p = 1.1
        let q = 1.0
        x <== p
        y <== q
        //printfn "%d" 1
        ctx.emit.comment "test001"
        //let z0 = ((6.2*(4.2/(-7.7)*(-2.1)))-y-6.8)
        //printfn "%d" <| 1
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((6.2*(4.2/(-7.7)*(-2.1)))-y-6.8)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((6.2*(4.2/(-7.7)*(-2.1)))-q-6.8)
            z2 <== ((6.2*(4.2/(-7.7)*(-2.1)))-y-6.8)
            wr.tt <| (I 1)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 2
        ctx.emit.comment "test002"
        //let z0 = (-x)
        //printfn "%d" <| 2
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 2)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 3
        ctx.emit.comment "test003"
        //let z0 = (-6.2)
        //printfn "%d" <| 3
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 4
        ctx.emit.comment "test004"
        //let z0 = 1.5
        //printfn "%d" <| 4
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 5
        ctx.emit.comment "test005"
        //let z0 = ((((-y)+5.6)+(y/(-y)-x+x/4.6)+((-x)))+(x/(y+(-y))+(-1.3)+x*y))
        //printfn "%d" <| 5
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-y)+5.6)+(y/(-y)-x+x/4.6)+((-x)))+(x/(y+(-y))+(-1.3)+x*y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-q)+5.6)+(q/(-q)-p+p/4.6)+((-p)))+(p/(q+(-q))+(-1.3)+p*q))
            z2 <== ((((-y)+5.6)+(y/(-y)-x+x/4.6)+((-x)))+(x/(y+(-y))+(-1.3)+x*y))
            wr.tt <| (I 5)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 6
        ctx.emit.comment "test006"
        //let z0 = (5.2/7.4+(((-6.6)*(-x)))*((y)/(-7.3)*((-y)-8.4*x-(-5.5)))-x)
        //printfn "%d" <| 6
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.2/7.4+(((-6.6)*(-x)))*((y)/(-7.3)*((-y)-8.4*x-(-5.5)))-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.2/7.4+(((-6.6)*(-p)))*((q)/(-7.3)*((-q)-8.4*p-(-5.5)))-p)
            z2 <== (5.2/7.4+(((-6.6)*(-x)))*((y)/(-7.3)*((-y)-8.4*x-(-5.5)))-x)
            wr.tt <| (I 6)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 7
        ctx.emit.comment "test007"
        //let z0 = ((y)+y+(0.8)*(-8.6)/7.1)
        //printfn "%d" <| 7
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y)+y+(0.8)*(-8.6)/7.1)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q)+q+(0.8)*(-8.6)/7.1)
            z2 <== ((y)+y+(0.8)*(-8.6)/7.1)
            wr.tt <| (I 7)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 8
        ctx.emit.comment "test008"
        //let z0 = 5.0
        //printfn "%d" <| 8
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 9
        ctx.emit.comment "test009"
        //let z0 = x
        //printfn "%d" <| 9
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 9)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 10
        ctx.emit.comment "test010"
        //let z0 = (y+((-8.5)/5.3+(4.6*(-y)+x-7.2))-(2.1-(8.1*(-x))*((-3.8)+1.2)+(-y)-(-2.7))*6.8+(y/4.0+6.3-0.0*y)+x*(-1.0))
        //printfn "%d" <| 10
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+((-8.5)/5.3+(4.6*(-y)+x-7.2))-(2.1-(8.1*(-x))*((-3.8)+1.2)+(-y)-(-2.7))*6.8+(y/4.0+6.3-0.0*y)+x*(-1.0))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+((-8.5)/5.3+(4.6*(-q)+p-7.2))-(2.1-(8.1*(-p))*((-3.8)+1.2)+(-q)-(-2.7))*6.8+(q/4.0+6.3-0.0*q)+p*(-1.0))
            z2 <== (y+((-8.5)/5.3+(4.6*(-y)+x-7.2))-(2.1-(8.1*(-x))*((-3.8)+1.2)+(-y)-(-2.7))*6.8+(y/4.0+6.3-0.0*y)+x*(-1.0))
            wr.tt <| (I 10)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 11
        ctx.emit.comment "test011"
        //let z0 = ((y*(-y)+(-1.1))*5.7)*8.2*(y*(-4.3)*((-3.4))-4.7)*((-y))+((-y))+x-y
        //printfn "%d" <| 11
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y*(-y)+(-1.1))*5.7)*8.2*(y*(-4.3)*((-3.4))-4.7)*((-y))+((-y))+x-y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q*(-q)+(-1.1))*5.7)*8.2*(q*(-4.3)*((-3.4))-4.7)*((-q))+((-q))+p-q
            z2 <== ((y*(-y)+(-1.1))*5.7)*8.2*(y*(-4.3)*((-3.4))-4.7)*((-y))+((-y))+x-y
            wr.tt <| (I 11)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 12
        ctx.emit.comment "test012"
        //let z0 = 2.2
        //printfn "%d" <| 12
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 13
        ctx.emit.comment "test013"
        //let z0 = (-8.5)
        //printfn "%d" <| 13
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 14
        ctx.emit.comment "test014"
        //let z0 = x
        //printfn "%d" <| 14
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 14)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 15
        ctx.emit.comment "test015"
        //let z0 = (-x)
        //printfn "%d" <| 15
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 15)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 16
        ctx.emit.comment "test016"
        //let z0 = 1.1
        //printfn "%d" <| 16
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 17
        ctx.emit.comment "test017"
        //let z0 = ((-x)/(((-x)*(-y)*(-y)*y)*(3.5)/8.7-(-1.6)))
        //printfn "%d" <| 17
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)/(((-x)*(-y)*(-y)*y)*(3.5)/8.7-(-1.6)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)/(((-p)*(-q)*(-q)*q)*(3.5)/8.7-(-1.6)))
            z2 <== ((-x)/(((-x)*(-y)*(-y)*y)*(3.5)/8.7-(-1.6)))
            wr.tt <| (I 17)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 18
        ctx.emit.comment "test018"
        //let z0 = (2.7-6.2)
        //printfn "%d" <| 18
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 19
        ctx.emit.comment "test019"
        //let z0 = ((7.0)+y*x-x)
        //printfn "%d" <| 19
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((7.0)+y*x-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((7.0)+q*p-p)
            z2 <== ((7.0)+y*x-x)
            wr.tt <| (I 19)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 20
        ctx.emit.comment "test020"
        //let z0 = (-4.2)
        //printfn "%d" <| 20
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 21
        ctx.emit.comment "test021"
        //let z0 = (((-0.4)+6.3*2.2-(-x))-0.7-(1.6/3.5*x)-(4.1*x+0.7/y*y))+((x/x/2.8))
        //printfn "%d" <| 21
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-0.4)+6.3*2.2-(-x))-0.7-(1.6/3.5*x)-(4.1*x+0.7/y*y))+((x/x/2.8))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-0.4)+6.3*2.2-(-p))-0.7-(1.6/3.5*p)-(4.1*p+0.7/q*q))+((p/p/2.8))
            z2 <== (((-0.4)+6.3*2.2-(-x))-0.7-(1.6/3.5*x)-(4.1*x+0.7/y*y))+((x/x/2.8))
            wr.tt <| (I 21)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 22
        ctx.emit.comment "test022"
        //let z0 = ((((-0.6)-(-2.5)+(-5.8)-(-y)))/5.8-((5.2*(-0.4))+4.3*(-x)*(y+y+7.5*1.8-7.2)))
        //printfn "%d" <| 22
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-0.6)-(-2.5)+(-5.8)-(-y)))/5.8-((5.2*(-0.4))+4.3*(-x)*(y+y+7.5*1.8-7.2)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-0.6)-(-2.5)+(-5.8)-(-q)))/5.8-((5.2*(-0.4))+4.3*(-p)*(q+q+7.5*1.8-7.2)))
            z2 <== ((((-0.6)-(-2.5)+(-5.8)-(-y)))/5.8-((5.2*(-0.4))+4.3*(-x)*(y+y+7.5*1.8-7.2)))
            wr.tt <| (I 22)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 23
        ctx.emit.comment "test023"
        //let z0 = (-x)
        //printfn "%d" <| 23
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 23)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 24
        ctx.emit.comment "test024"
        //let z0 = 6.1
        //printfn "%d" <| 24
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 25
        ctx.emit.comment "test025"
        //let z0 = ((-x))
        //printfn "%d" <| 25
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p))
            z2 <== ((-x))
            wr.tt <| (I 25)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 26
        ctx.emit.comment "test026"
        //let z0 = (x*(((-5.5)+(-4.5)*1.4/(-6.8)+(-y))*6.7*(x-(-x)-y-(-x))/((-y)/(-7.4)+y+(-y))/y)*2.8-y+(((-x)-y+(-0.7)/(-y))*2.7-(4.7+(-y))))
        //printfn "%d" <| 26
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x*(((-5.5)+(-4.5)*1.4/(-6.8)+(-y))*6.7*(x-(-x)-y-(-x))/((-y)/(-7.4)+y+(-y))/y)*2.8-y+(((-x)-y+(-0.7)/(-y))*2.7-(4.7+(-y))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p*(((-5.5)+(-4.5)*1.4/(-6.8)+(-q))*6.7*(p-(-p)-q-(-p))/((-q)/(-7.4)+q+(-q))/q)*2.8-q+(((-p)-q+(-0.7)/(-q))*2.7-(4.7+(-q))))
            z2 <== (x*(((-5.5)+(-4.5)*1.4/(-6.8)+(-y))*6.7*(x-(-x)-y-(-x))/((-y)/(-7.4)+y+(-y))/y)*2.8-y+(((-x)-y+(-0.7)/(-y))*2.7-(4.7+(-y))))
            wr.tt <| (I 26)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 27
        ctx.emit.comment "test027"
        //let z0 = (-0.3)
        //printfn "%d" <| 27
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 28
        ctx.emit.comment "test028"
        //let z0 = (-1.2)
        //printfn "%d" <| 28
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 29
        ctx.emit.comment "test029"
        //let z0 = 8.0
        //printfn "%d" <| 29
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 30
        ctx.emit.comment "test030"
        //let z0 = (-1.8)
        //printfn "%d" <| 30
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 31
        ctx.emit.comment "test031"
        //let z0 = (5.2*(-x))
        //printfn "%d" <| 31
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.2*(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.2*(-p))
            z2 <== (5.2*(-x))
            wr.tt <| (I 31)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 32
        ctx.emit.comment "test032"
        //let z0 = ((x-2.8)-(-y)-y)
        //printfn "%d" <| 32
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x-2.8)-(-y)-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p-2.8)-(-q)-q)
            z2 <== ((x-2.8)-(-y)-y)
            wr.tt <| (I 32)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 33
        ctx.emit.comment "test033"
        //let z0 = ((((-x)))*((-y)+((-y)-7.2))*((-2.4)+((-y)))+y+(-x))
        //printfn "%d" <| 33
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-x)))*((-y)+((-y)-7.2))*((-2.4)+((-y)))+y+(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-p)))*((-q)+((-q)-7.2))*((-2.4)+((-q)))+q+(-p))
            z2 <== ((((-x)))*((-y)+((-y)-7.2))*((-2.4)+((-y)))+y+(-x))
            wr.tt <| (I 33)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 34
        ctx.emit.comment "test034"
        //let z0 = 6.4
        //printfn "%d" <| 34
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 35
        ctx.emit.comment "test035"
        //let z0 = (x+(x-(-2.8)+y+(-5.8)/(-4.2))+(-3.5)-(-0.7)-(-6.8)*(x/(-y)+(-8.5))*(-7.3)-((-3.8)+(-y))-((6.7)-8.7-(-1.1)-(-7.0)+(-0.3)/1.0))
        //printfn "%d" <| 35
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x+(x-(-2.8)+y+(-5.8)/(-4.2))+(-3.5)-(-0.7)-(-6.8)*(x/(-y)+(-8.5))*(-7.3)-((-3.8)+(-y))-((6.7)-8.7-(-1.1)-(-7.0)+(-0.3)/1.0))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p+(p-(-2.8)+q+(-5.8)/(-4.2))+(-3.5)-(-0.7)-(-6.8)*(p/(-q)+(-8.5))*(-7.3)-((-3.8)+(-q))-((6.7)-8.7-(-1.1)-(-7.0)+(-0.3)/1.0))
            z2 <== (x+(x-(-2.8)+y+(-5.8)/(-4.2))+(-3.5)-(-0.7)-(-6.8)*(x/(-y)+(-8.5))*(-7.3)-((-3.8)+(-y))-((6.7)-8.7-(-1.1)-(-7.0)+(-0.3)/1.0))
            wr.tt <| (I 35)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 36
        ctx.emit.comment "test036"
        //let z0 = (-y)
        //printfn "%d" <| 36
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 36)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 37
        ctx.emit.comment "test037"
        //let z0 = (6.3-(-y)/5.8*7.1/(-y)+8.0-3.3)-((-x)-(-x)-(-8.7))+3.5-(-2.5)-3.0/1.7+(-x)*(-x)
        //printfn "%d" <| 37
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((6.3-(-y)/5.8*7.1/(-y)+8.0-3.3)-((-x)-(-x)-(-8.7))+3.5-(-2.5)-3.0/1.7+(-x)*(-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (6.3-(-q)/5.8*7.1/(-q)+8.0-3.3)-((-p)-(-p)-(-8.7))+3.5-(-2.5)-3.0/1.7+(-p)*(-p)
            z2 <== (6.3-(-y)/5.8*7.1/(-y)+8.0-3.3)-((-x)-(-x)-(-8.7))+3.5-(-2.5)-3.0/1.7+(-x)*(-x)
            wr.tt <| (I 37)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 38
        ctx.emit.comment "test038"
        //let z0 = (-2.2)
        //printfn "%d" <| 38
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 39
        ctx.emit.comment "test039"
        //let z0 = ((-x)+8.7*(x+(-y)-x)+(-y)*(y*(-x)-(-6.6)-x+y)*x*(x)/((-x)*y+(-5.5)/0.1))
        //printfn "%d" <| 39
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+8.7*(x+(-y)-x)+(-y)*(y*(-x)-(-6.6)-x+y)*x*(x)/((-x)*y+(-5.5)/0.1))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+8.7*(p+(-q)-p)+(-q)*(q*(-p)-(-6.6)-p+q)*p*(p)/((-p)*q+(-5.5)/0.1))
            z2 <== ((-x)+8.7*(x+(-y)-x)+(-y)*(y*(-x)-(-6.6)-x+y)*x*(x)/((-x)*y+(-5.5)/0.1))
            wr.tt <| (I 39)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 40
        ctx.emit.comment "test040"
        //let z0 = (-y)
        //printfn "%d" <| 40
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 40)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 41
        ctx.emit.comment "test041"
        //let z0 = (-2.4)
        //printfn "%d" <| 41
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 42
        ctx.emit.comment "test042"
        //let z0 = (-x)
        //printfn "%d" <| 42
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 42)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 43
        ctx.emit.comment "test043"
        //let z0 = (((y/(-y)*(-7.8)*x)+(-x)/(7.1))/y-(((-x)+(-2.8)-(-y)*(-4.4)*5.6)*(y+(-5.4)-(-7.0)/(-8.7))+3.4+6.1/1.1)*y*((-y))/(0.1*(-2.4)*(-7.2)-(-0.0))/(-y)-0.3)
        //printfn "%d" <| 43
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((y/(-y)*(-7.8)*x)+(-x)/(7.1))/y-(((-x)+(-2.8)-(-y)*(-4.4)*5.6)*(y+(-5.4)-(-7.0)/(-8.7))+3.4+6.1/1.1)*y*((-y))/(0.1*(-2.4)*(-7.2)-(-0.0))/(-y)-0.3)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((q/(-q)*(-7.8)*p)+(-p)/(7.1))/q-(((-p)+(-2.8)-(-q)*(-4.4)*5.6)*(q+(-5.4)-(-7.0)/(-8.7))+3.4+6.1/1.1)*q*((-q))/(0.1*(-2.4)*(-7.2)-(-0.0))/(-q)-0.3)
            z2 <== (((y/(-y)*(-7.8)*x)+(-x)/(7.1))/y-(((-x)+(-2.8)-(-y)*(-4.4)*5.6)*(y+(-5.4)-(-7.0)/(-8.7))+3.4+6.1/1.1)*y*((-y))/(0.1*(-2.4)*(-7.2)-(-0.0))/(-y)-0.3)
            wr.tt <| (I 43)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 44
        ctx.emit.comment "test044"
        //let z0 = (y*((-1.8)-(y*1.6*(-y))/(y-y)/(-y)+(-8.6)/y+y)-(x*(-0.1)/4.7)+(-0.0)+((-y)-(-y)+(-3.0))-((-6.6))/((-y)+(6.3+7.2-(-7.7)/(-x)/(-7.5))+(-x)+(-0.2)*8.3))
        //printfn "%d" <| 44
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y*((-1.8)-(y*1.6*(-y))/(y-y)/(-y)+(-8.6)/y+y)-(x*(-0.1)/4.7)+(-0.0)+((-y)-(-y)+(-3.0))-((-6.6))/((-y)+(6.3+7.2-(-7.7)/(-x)/(-7.5))+(-x)+(-0.2)*8.3))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q*((-1.8)-(q*1.6*(-q))/(q-q)/(-q)+(-8.6)/q+q)-(p*(-0.1)/4.7)+(-0.0)+((-q)-(-q)+(-3.0))-((-6.6))/((-q)+(6.3+7.2-(-7.7)/(-p)/(-7.5))+(-p)+(-0.2)*8.3))
            z2 <== (y*((-1.8)-(y*1.6*(-y))/(y-y)/(-y)+(-8.6)/y+y)-(x*(-0.1)/4.7)+(-0.0)+((-y)-(-y)+(-3.0))-((-6.6))/((-y)+(6.3+7.2-(-7.7)/(-x)/(-7.5))+(-x)+(-0.2)*8.3))
            wr.tt <| (I 44)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 45
        ctx.emit.comment "test045"
        //let z0 = (-x)
        //printfn "%d" <| 45
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 45)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 46
        ctx.emit.comment "test046"
        //let z0 = (-2.6)
        //printfn "%d" <| 46
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 47
        ctx.emit.comment "test047"
        //let z0 = ((-x)-y+(-5.2)*((-x))/(-x)-x/0.0+(x*4.6)/((-0.8)*(-y)))
        //printfn "%d" <| 47
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)-y+(-5.2)*((-x))/(-x)-x/0.0+(x*4.6)/((-0.8)*(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)-q+(-5.2)*((-p))/(-p)-p/0.0+(p*4.6)/((-0.8)*(-q)))
            z2 <== ((-x)-y+(-5.2)*((-x))/(-x)-x/0.0+(x*4.6)/((-0.8)*(-y)))
            wr.tt <| (I 47)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 48
        ctx.emit.comment "test048"
        //let z0 = (7.6+(4.7)/(x+y/0.1+(-0.7)+(-x))/(8.3*(-y)*(-x)/y/(-x))+x/x*x+(-x))
        //printfn "%d" <| 48
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.6+(4.7)/(x+y/0.1+(-0.7)+(-x))/(8.3*(-y)*(-x)/y/(-x))+x/x*x+(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.6+(4.7)/(p+q/0.1+(-0.7)+(-p))/(8.3*(-q)*(-p)/q/(-p))+p/p*p+(-p))
            z2 <== (7.6+(4.7)/(x+y/0.1+(-0.7)+(-x))/(8.3*(-y)*(-x)/y/(-x))+x/x*x+(-x))
            wr.tt <| (I 48)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 49
        ctx.emit.comment "test049"
        //let z0 = (-4.5)
        //printfn "%d" <| 49
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 50
        ctx.emit.comment "test050"
        //let z0 = (7.2/((-y)/(-x)/(-x)*y)/6.3*2.7-(-2.0)-1.1/(2.1-(-y)/(-5.8)*(-x)-x))
        //printfn "%d" <| 50
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.2/((-y)/(-x)/(-x)*y)/6.3*2.7-(-2.0)-1.1/(2.1-(-y)/(-5.8)*(-x)-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.2/((-q)/(-p)/(-p)*q)/6.3*2.7-(-2.0)-1.1/(2.1-(-q)/(-5.8)*(-p)-p))
            z2 <== (7.2/((-y)/(-x)/(-x)*y)/6.3*2.7-(-2.0)-1.1/(2.1-(-y)/(-5.8)*(-x)-x))
            wr.tt <| (I 50)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 51
        ctx.emit.comment "test051"
        //let z0 = 3.1
        //printfn "%d" <| 51
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 52
        ctx.emit.comment "test052"
        //let z0 = ((-7.7))
        //printfn "%d" <| 52
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 53
        ctx.emit.comment "test053"
        //let z0 = ((-x))
        //printfn "%d" <| 53
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p))
            z2 <== ((-x))
            wr.tt <| (I 53)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 54
        ctx.emit.comment "test054"
        //let z0 = ((-y)+(-x)*0.8/(-7.5))
        //printfn "%d" <| 54
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)+(-x)*0.8/(-7.5))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)+(-p)*0.8/(-7.5))
            z2 <== ((-y)+(-x)*0.8/(-7.5))
            wr.tt <| (I 54)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 55
        ctx.emit.comment "test055"
        //let z0 = (2.0)
        //printfn "%d" <| 55
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 56
        ctx.emit.comment "test056"
        //let z0 = ((((-x)*(-x)+1.4)/x*y/(2.4*(-4.5)))-x-x)
        //printfn "%d" <| 56
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-x)*(-x)+1.4)/x*y/(2.4*(-4.5)))-x-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-p)*(-p)+1.4)/p*q/(2.4*(-4.5)))-p-p)
            z2 <== ((((-x)*(-x)+1.4)/x*y/(2.4*(-4.5)))-x-x)
            wr.tt <| (I 56)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 57
        ctx.emit.comment "test057"
        //let z0 = (-y)
        //printfn "%d" <| 57
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 57)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 58
        ctx.emit.comment "test058"
        //let z0 = y
        //printfn "%d" <| 58
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 58)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 59
        ctx.emit.comment "test059"
        //let z0 = (-4.5)
        //printfn "%d" <| 59
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 60
        ctx.emit.comment "test060"
        //let z0 = (-7.4)
        //printfn "%d" <| 60
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 61
        ctx.emit.comment "test061"
        //let z0 = (((7.6*(-x)*x*(-x))+(y-1.6))*y)
        //printfn "%d" <| 61
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((7.6*(-x)*x*(-x))+(y-1.6))*y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((7.6*(-p)*p*(-p))+(q-1.6))*q)
            z2 <== (((7.6*(-x)*x*(-x))+(y-1.6))*y)
            wr.tt <| (I 61)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 62
        ctx.emit.comment "test062"
        //let z0 = (5.7-8.5*y)
        //printfn "%d" <| 62
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.7-8.5*y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.7-8.5*q)
            z2 <== (5.7-8.5*y)
            wr.tt <| (I 62)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 63
        ctx.emit.comment "test063"
        //let z0 = (x/x)
        //printfn "%d" <| 63
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x/x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p/p)
            z2 <== (x/x)
            wr.tt <| (I 63)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 64
        ctx.emit.comment "test064"
        //let z0 = x
        //printfn "%d" <| 64
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 64)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 65
        ctx.emit.comment "test065"
        //let z0 = (-y)
        //printfn "%d" <| 65
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 65)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 66
        ctx.emit.comment "test066"
        //let z0 = (1.6-y)
        //printfn "%d" <| 66
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((1.6-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (1.6-q)
            z2 <== (1.6-y)
            wr.tt <| (I 66)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 67
        ctx.emit.comment "test067"
        //let z0 = (-x)
        //printfn "%d" <| 67
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 67)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 68
        ctx.emit.comment "test068"
        //let z0 = y
        //printfn "%d" <| 68
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 68)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 69
        ctx.emit.comment "test069"
        //let z0 = x
        //printfn "%d" <| 69
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 69)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 70
        ctx.emit.comment "test070"
        //let z0 = ((-6.7)/(4.2*(-1.1)*y)*(-x)*x-x-((-x)+x/y)-y+(-4.2)*x)
        //printfn "%d" <| 70
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-6.7)/(4.2*(-1.1)*y)*(-x)*x-x-((-x)+x/y)-y+(-4.2)*x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-6.7)/(4.2*(-1.1)*q)*(-p)*p-p-((-p)+p/q)-q+(-4.2)*p)
            z2 <== ((-6.7)/(4.2*(-1.1)*y)*(-x)*x-x-((-x)+x/y)-y+(-4.2)*x)
            wr.tt <| (I 70)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 71
        ctx.emit.comment "test071"
        //let z0 = (-x)
        //printfn "%d" <| 71
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 71)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 72
        ctx.emit.comment "test072"
        //let z0 = ((-5.1)/(-y)*(-x)-(7.5-(-3.1)-(-2.0)+(-y)-(-6.1)))
        //printfn "%d" <| 72
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-5.1)/(-y)*(-x)-(7.5-(-3.1)-(-2.0)+(-y)-(-6.1)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-5.1)/(-q)*(-p)-(7.5-(-3.1)-(-2.0)+(-q)-(-6.1)))
            z2 <== ((-5.1)/(-y)*(-x)-(7.5-(-3.1)-(-2.0)+(-y)-(-6.1)))
            wr.tt <| (I 72)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 73
        ctx.emit.comment "test073"
        //let z0 = (-1.3)
        //printfn "%d" <| 73
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 74
        ctx.emit.comment "test074"
        //let z0 = y
        //printfn "%d" <| 74
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 74)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 75
        ctx.emit.comment "test075"
        //let z0 = 5.4
        //printfn "%d" <| 75
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 76
        ctx.emit.comment "test076"
        //let z0 = 6.3
        //printfn "%d" <| 76
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 77
        ctx.emit.comment "test077"
        //let z0 = 8.1
        //printfn "%d" <| 77
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 78
        ctx.emit.comment "test078"
        //let z0 = y
        //printfn "%d" <| 78
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 78)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 79
        ctx.emit.comment "test079"
        //let z0 = ((x-1.6*7.7)*(x/(-3.5)+(-x))+(y*(-8.4)+0.4-x)/((-y)*y)*(-y)*(-8.1)-(-8.2)+3.0*(-x)*x/(-7.7)/(-1.8)-3.4/(2.0+(-y))-x-y)
        //printfn "%d" <| 79
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x-1.6*7.7)*(x/(-3.5)+(-x))+(y*(-8.4)+0.4-x)/((-y)*y)*(-y)*(-8.1)-(-8.2)+3.0*(-x)*x/(-7.7)/(-1.8)-3.4/(2.0+(-y))-x-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p-1.6*7.7)*(p/(-3.5)+(-p))+(q*(-8.4)+0.4-p)/((-q)*q)*(-q)*(-8.1)-(-8.2)+3.0*(-p)*p/(-7.7)/(-1.8)-3.4/(2.0+(-q))-p-q)
            z2 <== ((x-1.6*7.7)*(x/(-3.5)+(-x))+(y*(-8.4)+0.4-x)/((-y)*y)*(-y)*(-8.1)-(-8.2)+3.0*(-x)*x/(-7.7)/(-1.8)-3.4/(2.0+(-y))-x-y)
            wr.tt <| (I 79)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 80
        ctx.emit.comment "test080"
        //let z0 = ((-0.4))
        //printfn "%d" <| 80
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 81
        ctx.emit.comment "test081"
        //let z0 = (x/(-3.0))
        //printfn "%d" <| 81
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x/(-3.0))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p/(-3.0))
            z2 <== (x/(-3.0))
            wr.tt <| (I 81)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 82
        ctx.emit.comment "test082"
        //let z0 = (y/(3.2/(-5.1)/y)-(7.7*y*(-y))/(5.0-(8.4*(-y)+(-y)*y)+(-1.6)-y+(-x)-4.1-5.2)-((-8.4)*x-(-y)+(-x)-(-4.5))+(-3.4)*(-y)+y-y*((y*y/2.0)))
        //printfn "%d" <| 82
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y/(3.2/(-5.1)/y)-(7.7*y*(-y))/(5.0-(8.4*(-y)+(-y)*y)+(-1.6)-y+(-x)-4.1-5.2)-((-8.4)*x-(-y)+(-x)-(-4.5))+(-3.4)*(-y)+y-y*((y*y/2.0)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q/(3.2/(-5.1)/q)-(7.7*q*(-q))/(5.0-(8.4*(-q)+(-q)*q)+(-1.6)-q+(-p)-4.1-5.2)-((-8.4)*p-(-q)+(-p)-(-4.5))+(-3.4)*(-q)+q-q*((q*q/2.0)))
            z2 <== (y/(3.2/(-5.1)/y)-(7.7*y*(-y))/(5.0-(8.4*(-y)+(-y)*y)+(-1.6)-y+(-x)-4.1-5.2)-((-8.4)*x-(-y)+(-x)-(-4.5))+(-3.4)*(-y)+y-y*((y*y/2.0)))
            wr.tt <| (I 82)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 83
        ctx.emit.comment "test083"
        //let z0 = (-7.7)
        //printfn "%d" <| 83
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 84
        ctx.emit.comment "test084"
        //let z0 = ((-7.7))
        //printfn "%d" <| 84
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 85
        ctx.emit.comment "test085"
        //let z0 = x
        //printfn "%d" <| 85
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 85)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 86
        ctx.emit.comment "test086"
        //let z0 = (7.5*0.1/((-x)-6.8/(-y))/y-(-y)+(5.4*x-(-y)-(-y)*(-8.7)))
        //printfn "%d" <| 86
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.5*0.1/((-x)-6.8/(-y))/y-(-y)+(5.4*x-(-y)-(-y)*(-8.7)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.5*0.1/((-p)-6.8/(-q))/q-(-q)+(5.4*p-(-q)-(-q)*(-8.7)))
            z2 <== (7.5*0.1/((-x)-6.8/(-y))/y-(-y)+(5.4*x-(-y)-(-y)*(-8.7)))
            wr.tt <| (I 86)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 87
        ctx.emit.comment "test087"
        //let z0 = ((-5.6))
        //printfn "%d" <| 87
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 88
        ctx.emit.comment "test088"
        //let z0 = y
        //printfn "%d" <| 88
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 88)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 89
        ctx.emit.comment "test089"
        //let z0 = y
        //printfn "%d" <| 89
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 89)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 90
        ctx.emit.comment "test090"
        //let z0 = (-4.1)
        //printfn "%d" <| 90
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 91
        ctx.emit.comment "test091"
        //let z0 = y
        //printfn "%d" <| 91
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 91)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 92
        ctx.emit.comment "test092"
        //let z0 = (-x)
        //printfn "%d" <| 92
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 92)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 93
        ctx.emit.comment "test093"
        //let z0 = (1.3-3.6+y-5.5)
        //printfn "%d" <| 93
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((1.3-3.6+y-5.5)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (1.3-3.6+q-5.5)
            z2 <== (1.3-3.6+y-5.5)
            wr.tt <| (I 93)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 94
        ctx.emit.comment "test094"
        //let z0 = (-5.7)
        //printfn "%d" <| 94
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 95
        ctx.emit.comment "test095"
        //let z0 = (((0.4/1.5)*(x+(-y)*y-2.6/8.7)+4.0-2.2*(-x)))
        //printfn "%d" <| 95
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((0.4/1.5)*(x+(-y)*y-2.6/8.7)+4.0-2.2*(-x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((0.4/1.5)*(p+(-q)*q-2.6/8.7)+4.0-2.2*(-p)))
            z2 <== (((0.4/1.5)*(x+(-y)*y-2.6/8.7)+4.0-2.2*(-x)))
            wr.tt <| (I 95)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 96
        ctx.emit.comment "test096"
        //let z0 = (-y)
        //printfn "%d" <| 96
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 96)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 97
        ctx.emit.comment "test097"
        //let z0 = ((((-0.3)/(-4.0)*(-x)+5.0)-x*(-x)-(-0.7))/((-x)*5.4-8.1/(x)+((-y)-(-y)/6.7/y))/((-y)*1.6*y*(-y)+x+(-y)-((-2.1)+x/2.6-(-y)+(-x))+x/x+x/(-5.7))*((x/(-x)*(-y)-(-y)/(-7.3))*x*1.3))
        //printfn "%d" <| 97
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-0.3)/(-4.0)*(-x)+5.0)-x*(-x)-(-0.7))/((-x)*5.4-8.1/(x)+((-y)-(-y)/6.7/y))/((-y)*1.6*y*(-y)+x+(-y)-((-2.1)+x/2.6-(-y)+(-x))+x/x+x/(-5.7))*((x/(-x)*(-y)-(-y)/(-7.3))*x*1.3))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-0.3)/(-4.0)*(-p)+5.0)-p*(-p)-(-0.7))/((-p)*5.4-8.1/(p)+((-q)-(-q)/6.7/q))/((-q)*1.6*q*(-q)+p+(-q)-((-2.1)+p/2.6-(-q)+(-p))+p/p+p/(-5.7))*((p/(-p)*(-q)-(-q)/(-7.3))*p*1.3))
            z2 <== ((((-0.3)/(-4.0)*(-x)+5.0)-x*(-x)-(-0.7))/((-x)*5.4-8.1/(x)+((-y)-(-y)/6.7/y))/((-y)*1.6*y*(-y)+x+(-y)-((-2.1)+x/2.6-(-y)+(-x))+x/x+x/(-5.7))*((x/(-x)*(-y)-(-y)/(-7.3))*x*1.3))
            wr.tt <| (I 97)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 98
        ctx.emit.comment "test098"
        //let z0 = (((x-(-0.6)+(-0.0)+x/(-0.6))*y+(-0.0)/((-5.4)/x))/((-1.7)*(-0.2)*4.8+(-1.4)-x)+0.3-((-8.3)-(-x))*((-7.8)+(-y))/(1.3)*(0.4-(-x)/(2.5*(-3.7)+4.1)-(7.5+4.0*2.8/(-y)-(-4.3))/(-2.7)))
        //printfn "%d" <| 98
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((x-(-0.6)+(-0.0)+x/(-0.6))*y+(-0.0)/((-5.4)/x))/((-1.7)*(-0.2)*4.8+(-1.4)-x)+0.3-((-8.3)-(-x))*((-7.8)+(-y))/(1.3)*(0.4-(-x)/(2.5*(-3.7)+4.1)-(7.5+4.0*2.8/(-y)-(-4.3))/(-2.7)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((p-(-0.6)+(-0.0)+p/(-0.6))*q+(-0.0)/((-5.4)/p))/((-1.7)*(-0.2)*4.8+(-1.4)-p)+0.3-((-8.3)-(-p))*((-7.8)+(-q))/(1.3)*(0.4-(-p)/(2.5*(-3.7)+4.1)-(7.5+4.0*2.8/(-q)-(-4.3))/(-2.7)))
            z2 <== (((x-(-0.6)+(-0.0)+x/(-0.6))*y+(-0.0)/((-5.4)/x))/((-1.7)*(-0.2)*4.8+(-1.4)-x)+0.3-((-8.3)-(-x))*((-7.8)+(-y))/(1.3)*(0.4-(-x)/(2.5*(-3.7)+4.1)-(7.5+4.0*2.8/(-y)-(-4.3))/(-2.7)))
            wr.tt <| (I 98)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 99
        ctx.emit.comment "test099"
        //let z0 = 3.0
        //printfn "%d" <| 99
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 100
        ctx.emit.comment "test100"
        //let z0 = 2.7
        //printfn "%d" <| 100
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 101
        ctx.emit.comment "test101"
        //let z0 = 8.4
        //printfn "%d" <| 101
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 102
        ctx.emit.comment "test102"
        //let z0 = (x)
        //printfn "%d" <| 102
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p)
            z2 <== (x)
            wr.tt <| (I 102)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 103
        ctx.emit.comment "test103"
        //let z0 = x
        //printfn "%d" <| 103
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 103)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 104
        ctx.emit.comment "test104"
        //let z0 = y
        //printfn "%d" <| 104
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 104)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 105
        ctx.emit.comment "test105"
        //let z0 = (-x)
        //printfn "%d" <| 105
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 105)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 106
        ctx.emit.comment "test106"
        //let z0 = (-4.6)
        //printfn "%d" <| 106
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 107
        ctx.emit.comment "test107"
        //let z0 = x
        //printfn "%d" <| 107
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 107)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 108
        ctx.emit.comment "test108"
        //let z0 = ((-5.4)/5.0+(y/3.6-x)-3.4/0.6+y*x-((-1.0)+(-x)+1.1/(-4.8)/4.0)/((-7.6)+y/3.5*(-y)-(-x)))
        //printfn "%d" <| 108
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-5.4)/5.0+(y/3.6-x)-3.4/0.6+y*x-((-1.0)+(-x)+1.1/(-4.8)/4.0)/((-7.6)+y/3.5*(-y)-(-x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-5.4)/5.0+(q/3.6-p)-3.4/0.6+q*p-((-1.0)+(-p)+1.1/(-4.8)/4.0)/((-7.6)+q/3.5*(-q)-(-p)))
            z2 <== ((-5.4)/5.0+(y/3.6-x)-3.4/0.6+y*x-((-1.0)+(-x)+1.1/(-4.8)/4.0)/((-7.6)+y/3.5*(-y)-(-x)))
            wr.tt <| (I 108)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 109
        ctx.emit.comment "test109"
        //let z0 = ((1.7+(-6.8))*y+((8.7-7.0)*((-6.6)/8.7-(-x)/y/y)*(-1.1)+(-1.2))-(((-7.8))-(-y)+x+(3.4/(-1.5)/(-y)-(-y))+(-7.5)/3.1)*((-6.6)+(-y)-(8.1/(-7.1))))
        //printfn "%d" <| 109
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((1.7+(-6.8))*y+((8.7-7.0)*((-6.6)/8.7-(-x)/y/y)*(-1.1)+(-1.2))-(((-7.8))-(-y)+x+(3.4/(-1.5)/(-y)-(-y))+(-7.5)/3.1)*((-6.6)+(-y)-(8.1/(-7.1))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((1.7+(-6.8))*q+((8.7-7.0)*((-6.6)/8.7-(-p)/q/q)*(-1.1)+(-1.2))-(((-7.8))-(-q)+p+(3.4/(-1.5)/(-q)-(-q))+(-7.5)/3.1)*((-6.6)+(-q)-(8.1/(-7.1))))
            z2 <== ((1.7+(-6.8))*y+((8.7-7.0)*((-6.6)/8.7-(-x)/y/y)*(-1.1)+(-1.2))-(((-7.8))-(-y)+x+(3.4/(-1.5)/(-y)-(-y))+(-7.5)/3.1)*((-6.6)+(-y)-(8.1/(-7.1))))
            wr.tt <| (I 109)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 110
        ctx.emit.comment "test110"
        //let z0 = (-3.4)
        //printfn "%d" <| 110
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 111
        ctx.emit.comment "test111"
        //let z0 = ((-y)/0.3/6.0)
        //printfn "%d" <| 111
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)/0.3/6.0)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)/0.3/6.0)
            z2 <== ((-y)/0.3/6.0)
            wr.tt <| (I 111)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 112
        ctx.emit.comment "test112"
        //let z0 = y
        //printfn "%d" <| 112
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 112)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 113
        ctx.emit.comment "test113"
        //let z0 = (2.4)
        //printfn "%d" <| 113
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 114
        ctx.emit.comment "test114"
        //let z0 = ((((-y)/(-y)/x)*((-5.0)-y/(-x)-(-y))+y-6.6-3.1)/(8.6+8.5*y+(-1.2)*5.6)-((-8.0)-(7.6*4.5)))
        //printfn "%d" <| 114
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-y)/(-y)/x)*((-5.0)-y/(-x)-(-y))+y-6.6-3.1)/(8.6+8.5*y+(-1.2)*5.6)-((-8.0)-(7.6*4.5)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-q)/(-q)/p)*((-5.0)-q/(-p)-(-q))+q-6.6-3.1)/(8.6+8.5*q+(-1.2)*5.6)-((-8.0)-(7.6*4.5)))
            z2 <== ((((-y)/(-y)/x)*((-5.0)-y/(-x)-(-y))+y-6.6-3.1)/(8.6+8.5*y+(-1.2)*5.6)-((-8.0)-(7.6*4.5)))
            wr.tt <| (I 114)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 115
        ctx.emit.comment "test115"
        //let z0 = ((-6.0)-(((-y)))/y*((-6.1))*(-x)*2.2*((x-1.1+(-y)+(-y))+(-y)*(7.2))*(-0.7))
        //printfn "%d" <| 115
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-6.0)-(((-y)))/y*((-6.1))*(-x)*2.2*((x-1.1+(-y)+(-y))+(-y)*(7.2))*(-0.7))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-6.0)-(((-q)))/q*((-6.1))*(-p)*2.2*((p-1.1+(-q)+(-q))+(-q)*(7.2))*(-0.7))
            z2 <== ((-6.0)-(((-y)))/y*((-6.1))*(-x)*2.2*((x-1.1+(-y)+(-y))+(-y)*(7.2))*(-0.7))
            wr.tt <| (I 115)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 116
        ctx.emit.comment "test116"
        //let z0 = (y+4.0-((-2.7)/((-7.3)/1.2)-((-y)*(-y))-((-y)/7.0*y)*((-x)*4.0/8.2+(-3.4)-(-7.4)))/7.2*(0.4+(x)))
        //printfn "%d" <| 116
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+4.0-((-2.7)/((-7.3)/1.2)-((-y)*(-y))-((-y)/7.0*y)*((-x)*4.0/8.2+(-3.4)-(-7.4)))/7.2*(0.4+(x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+4.0-((-2.7)/((-7.3)/1.2)-((-q)*(-q))-((-q)/7.0*q)*((-p)*4.0/8.2+(-3.4)-(-7.4)))/7.2*(0.4+(p)))
            z2 <== (y+4.0-((-2.7)/((-7.3)/1.2)-((-y)*(-y))-((-y)/7.0*y)*((-x)*4.0/8.2+(-3.4)-(-7.4)))/7.2*(0.4+(x)))
            wr.tt <| (I 116)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 117
        ctx.emit.comment "test117"
        //let z0 = (0.0/(-x))
        //printfn "%d" <| 117
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((0.0/(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (0.0/(-p))
            z2 <== (0.0/(-x))
            wr.tt <| (I 117)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 118
        ctx.emit.comment "test118"
        //let z0 = y
        //printfn "%d" <| 118
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 118)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 119
        ctx.emit.comment "test119"
        //let z0 = ((((-x)/(-y)+y-y/7.0))+(y-((-8.1))/x))
        //printfn "%d" <| 119
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-x)/(-y)+y-y/7.0))+(y-((-8.1))/x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-p)/(-q)+q-q/7.0))+(q-((-8.1))/p))
            z2 <== ((((-x)/(-y)+y-y/7.0))+(y-((-8.1))/x))
            wr.tt <| (I 119)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 120
        ctx.emit.comment "test120"
        //let z0 = (-x)
        //printfn "%d" <| 120
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 120)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 121
        ctx.emit.comment "test121"
        //let z0 = 2.6
        //printfn "%d" <| 121
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 122
        ctx.emit.comment "test122"
        //let z0 = ((-0.5)*x*y)-((-x)/y)/((-x)/(-y))
        //printfn "%d" <| 122
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.5)*x*y)-((-x)/y)/((-x)/(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.5)*p*q)-((-p)/q)/((-p)/(-q))
            z2 <== ((-0.5)*x*y)-((-x)/y)/((-x)/(-y))
            wr.tt <| (I 122)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 123
        ctx.emit.comment "test123"
        //let z0 = (x/(2.6*(-3.6)+((-y)/6.2*(-y)*(-y)*(-x)))/(x/(-3.7)/(-y)/((-8.0)-4.3+(-1.0)*(-2.2))*(-x)))
        //printfn "%d" <| 123
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x/(2.6*(-3.6)+((-y)/6.2*(-y)*(-y)*(-x)))/(x/(-3.7)/(-y)/((-8.0)-4.3+(-1.0)*(-2.2))*(-x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p/(2.6*(-3.6)+((-q)/6.2*(-q)*(-q)*(-p)))/(p/(-3.7)/(-q)/((-8.0)-4.3+(-1.0)*(-2.2))*(-p)))
            z2 <== (x/(2.6*(-3.6)+((-y)/6.2*(-y)*(-y)*(-x)))/(x/(-3.7)/(-y)/((-8.0)-4.3+(-1.0)*(-2.2))*(-x)))
            wr.tt <| (I 123)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 124
        ctx.emit.comment "test124"
        //let z0 = ((y+7.6*(-5.5)/(-2.2))*((-y)*6.8*(-3.3)*8.7)/(-1.5))-1.1-x*((-5.6)/y-((-2.6)-1.5*y-0.5+(-4.2))*y)+y
        //printfn "%d" <| 124
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y+7.6*(-5.5)/(-2.2))*((-y)*6.8*(-3.3)*8.7)/(-1.5))-1.1-x*((-5.6)/y-((-2.6)-1.5*y-0.5+(-4.2))*y)+y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q+7.6*(-5.5)/(-2.2))*((-q)*6.8*(-3.3)*8.7)/(-1.5))-1.1-p*((-5.6)/q-((-2.6)-1.5*q-0.5+(-4.2))*q)+q
            z2 <== ((y+7.6*(-5.5)/(-2.2))*((-y)*6.8*(-3.3)*8.7)/(-1.5))-1.1-x*((-5.6)/y-((-2.6)-1.5*y-0.5+(-4.2))*y)+y
            wr.tt <| (I 124)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 125
        ctx.emit.comment "test125"
        //let z0 = y
        //printfn "%d" <| 125
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 125)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 126
        ctx.emit.comment "test126"
        //let z0 = y
        //printfn "%d" <| 126
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 126)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 127
        ctx.emit.comment "test127"
        //let z0 = (-5.4)
        //printfn "%d" <| 127
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 128
        ctx.emit.comment "test128"
        //let z0 = (-0.8)
        //printfn "%d" <| 128
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 129
        ctx.emit.comment "test129"
        //let z0 = (((-0.8)*6.1+(-y)/(-x)*8.5))
        //printfn "%d" <| 129
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-0.8)*6.1+(-y)/(-x)*8.5))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-0.8)*6.1+(-q)/(-p)*8.5))
            z2 <== (((-0.8)*6.1+(-y)/(-x)*8.5))
            wr.tt <| (I 129)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 130
        ctx.emit.comment "test130"
        //let z0 = x
        //printfn "%d" <| 130
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 130)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 131
        ctx.emit.comment "test131"
        //let z0 = (-6.6)
        //printfn "%d" <| 131
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 132
        ctx.emit.comment "test132"
        //let z0 = ((((-x)/(-3.7)-(-5.3)+(-5.5))*((-6.3)-(-0.8))/(y)-8.2+5.7/7.2+((-x)))*(x-(y/0.3-5.5-(-7.7))/(6.0))-(((-y)-(-x)+(-x))*(-y)*(-7.7)))
        //printfn "%d" <| 132
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-x)/(-3.7)-(-5.3)+(-5.5))*((-6.3)-(-0.8))/(y)-8.2+5.7/7.2+((-x)))*(x-(y/0.3-5.5-(-7.7))/(6.0))-(((-y)-(-x)+(-x))*(-y)*(-7.7)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-p)/(-3.7)-(-5.3)+(-5.5))*((-6.3)-(-0.8))/(q)-8.2+5.7/7.2+((-p)))*(p-(q/0.3-5.5-(-7.7))/(6.0))-(((-q)-(-p)+(-p))*(-q)*(-7.7)))
            z2 <== ((((-x)/(-3.7)-(-5.3)+(-5.5))*((-6.3)-(-0.8))/(y)-8.2+5.7/7.2+((-x)))*(x-(y/0.3-5.5-(-7.7))/(6.0))-(((-y)-(-x)+(-x))*(-y)*(-7.7)))
            wr.tt <| (I 132)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 133
        ctx.emit.comment "test133"
        //let z0 = ((6.0+(-x)/(-1.2)/(-y)))
        //printfn "%d" <| 133
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((6.0+(-x)/(-1.2)/(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((6.0+(-p)/(-1.2)/(-q)))
            z2 <== ((6.0+(-x)/(-1.2)/(-y)))
            wr.tt <| (I 133)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 134
        ctx.emit.comment "test134"
        //let z0 = (-x)
        //printfn "%d" <| 134
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 134)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 135
        ctx.emit.comment "test135"
        //let z0 = (-x)
        //printfn "%d" <| 135
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 135)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 136
        ctx.emit.comment "test136"
        //let z0 = ((-7.4)-((-0.3)+((-x))*(y*2.6*(-x)+(-x)-(-x)))*(-x))
        //printfn "%d" <| 136
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.4)-((-0.3)+((-x))*(y*2.6*(-x)+(-x)-(-x)))*(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.4)-((-0.3)+((-p))*(q*2.6*(-p)+(-p)-(-p)))*(-p))
            z2 <== ((-7.4)-((-0.3)+((-x))*(y*2.6*(-x)+(-x)-(-x)))*(-x))
            wr.tt <| (I 136)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 137
        ctx.emit.comment "test137"
        //let z0 = (-y)
        //printfn "%d" <| 137
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 137)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 138
        ctx.emit.comment "test138"
        //let z0 = 4.4
        //printfn "%d" <| 138
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 139
        ctx.emit.comment "test139"
        //let z0 = (-x)
        //printfn "%d" <| 139
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 139)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 140
        ctx.emit.comment "test140"
        //let z0 = ((-y)-(((-x)/1.6/y)-(-y)-(-y))+3.5/((-8.5)/(-2.7)/4.7)*x)
        //printfn "%d" <| 140
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)-(((-x)/1.6/y)-(-y)-(-y))+3.5/((-8.5)/(-2.7)/4.7)*x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)-(((-p)/1.6/q)-(-q)-(-q))+3.5/((-8.5)/(-2.7)/4.7)*p)
            z2 <== ((-y)-(((-x)/1.6/y)-(-y)-(-y))+3.5/((-8.5)/(-2.7)/4.7)*x)
            wr.tt <| (I 140)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 141
        ctx.emit.comment "test141"
        //let z0 = ((-y)-2.0/x+(x+(-x)*y))
        //printfn "%d" <| 141
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)-2.0/x+(x+(-x)*y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)-2.0/p+(p+(-p)*q))
            z2 <== ((-y)-2.0/x+(x+(-x)*y))
            wr.tt <| (I 141)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 142
        ctx.emit.comment "test142"
        //let z0 = x
        //printfn "%d" <| 142
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 142)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 143
        ctx.emit.comment "test143"
        //let z0 = ((((-y)-1.5/6.0/8.4)))
        //printfn "%d" <| 143
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-y)-1.5/6.0/8.4)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-q)-1.5/6.0/8.4)))
            z2 <== ((((-y)-1.5/6.0/8.4)))
            wr.tt <| (I 143)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 144
        ctx.emit.comment "test144"
        //let z0 = (-6.4)
        //printfn "%d" <| 144
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 145
        ctx.emit.comment "test145"
        //let z0 = (-y)
        //printfn "%d" <| 145
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 145)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 146
        ctx.emit.comment "test146"
        //let z0 = (7.4-((-1.3)*0.0*(-x)-(-y)-(-y))/1.0*x/(((-y)+(-8.5)+(-5.6)*6.0)*((-7.3)*(-y)*(-x)/y-(-7.8))*2.3+x*((-y)-2.8+(-2.7)+y))*y-(0.5+x*((-5.8)*5.0-(-3.0)+0.0)+((-x)/y-7.3-(-6.2)*(-0.0))+((-x))))
        //printfn "%d" <| 146
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.4-((-1.3)*0.0*(-x)-(-y)-(-y))/1.0*x/(((-y)+(-8.5)+(-5.6)*6.0)*((-7.3)*(-y)*(-x)/y-(-7.8))*2.3+x*((-y)-2.8+(-2.7)+y))*y-(0.5+x*((-5.8)*5.0-(-3.0)+0.0)+((-x)/y-7.3-(-6.2)*(-0.0))+((-x))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.4-((-1.3)*0.0*(-p)-(-q)-(-q))/1.0*p/(((-q)+(-8.5)+(-5.6)*6.0)*((-7.3)*(-q)*(-p)/q-(-7.8))*2.3+p*((-q)-2.8+(-2.7)+q))*q-(0.5+p*((-5.8)*5.0-(-3.0)+0.0)+((-p)/q-7.3-(-6.2)*(-0.0))+((-p))))
            z2 <== (7.4-((-1.3)*0.0*(-x)-(-y)-(-y))/1.0*x/(((-y)+(-8.5)+(-5.6)*6.0)*((-7.3)*(-y)*(-x)/y-(-7.8))*2.3+x*((-y)-2.8+(-2.7)+y))*y-(0.5+x*((-5.8)*5.0-(-3.0)+0.0)+((-x)/y-7.3-(-6.2)*(-0.0))+((-x))))
            wr.tt <| (I 146)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 147
        ctx.emit.comment "test147"
        //let z0 = (y-(-x)/x*(-x)/(y*(-8.5)/2.1+(-1.8)/(-2.1))*0.7+((5.4/y*(-x)+x)/((-3.7)-(-y)*2.1+3.7*(-x))-(-y)/(-y)-2.3/8.6*3.1*(-y)/(-3.2)))
        //printfn "%d" <| 147
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y-(-x)/x*(-x)/(y*(-8.5)/2.1+(-1.8)/(-2.1))*0.7+((5.4/y*(-x)+x)/((-3.7)-(-y)*2.1+3.7*(-x))-(-y)/(-y)-2.3/8.6*3.1*(-y)/(-3.2)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q-(-p)/p*(-p)/(q*(-8.5)/2.1+(-1.8)/(-2.1))*0.7+((5.4/q*(-p)+p)/((-3.7)-(-q)*2.1+3.7*(-p))-(-q)/(-q)-2.3/8.6*3.1*(-q)/(-3.2)))
            z2 <== (y-(-x)/x*(-x)/(y*(-8.5)/2.1+(-1.8)/(-2.1))*0.7+((5.4/y*(-x)+x)/((-3.7)-(-y)*2.1+3.7*(-x))-(-y)/(-y)-2.3/8.6*3.1*(-y)/(-3.2)))
            wr.tt <| (I 147)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 148
        ctx.emit.comment "test148"
        //let z0 = ((-7.5)/(-y)+y)
        //printfn "%d" <| 148
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.5)/(-y)+y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.5)/(-q)+q)
            z2 <== ((-7.5)/(-y)+y)
            wr.tt <| (I 148)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 149
        ctx.emit.comment "test149"
        //let z0 = y/(-x)/(((-2.6)*(-x)-y)+(x/(-y)*x/y+(-x))-(-y))/6.2-((-5.7)*x)+((6.4)-(-y))
        //printfn "%d" <| 149
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y/(-x)/(((-2.6)*(-x)-y)+(x/(-y)*x/y+(-x))-(-y))/6.2-((-5.7)*x)+((6.4)-(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q/(-p)/(((-2.6)*(-p)-q)+(p/(-q)*p/q+(-p))-(-q))/6.2-((-5.7)*p)+((6.4)-(-q))
            z2 <== y/(-x)/(((-2.6)*(-x)-y)+(x/(-y)*x/y+(-x))-(-y))/6.2-((-5.7)*x)+((6.4)-(-y))
            wr.tt <| (I 149)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 150
        ctx.emit.comment "test150"
        //let z0 = y
        //printfn "%d" <| 150
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 150)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 151
        ctx.emit.comment "test151"
        //let z0 = (((-5.4)+2.7)*(y/(-2.7)+7.1)-(y/((-x)/y*(-5.6)+(-x)+(-x))*((-x)+(-x)*(-0.4)+2.7)-((-x)*x*(-3.1)*7.7)+((-y)*(-6.0)))/(-y)-((-3.8)/(-5.3)/y)+((-4.5))-(0.2-y)*((-6.4)*3.3+(-8.3))+8.8)
        //printfn "%d" <| 151
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-5.4)+2.7)*(y/(-2.7)+7.1)-(y/((-x)/y*(-5.6)+(-x)+(-x))*((-x)+(-x)*(-0.4)+2.7)-((-x)*x*(-3.1)*7.7)+((-y)*(-6.0)))/(-y)-((-3.8)/(-5.3)/y)+((-4.5))-(0.2-y)*((-6.4)*3.3+(-8.3))+8.8)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-5.4)+2.7)*(q/(-2.7)+7.1)-(q/((-p)/q*(-5.6)+(-p)+(-p))*((-p)+(-p)*(-0.4)+2.7)-((-p)*p*(-3.1)*7.7)+((-q)*(-6.0)))/(-q)-((-3.8)/(-5.3)/q)+((-4.5))-(0.2-q)*((-6.4)*3.3+(-8.3))+8.8)
            z2 <== (((-5.4)+2.7)*(y/(-2.7)+7.1)-(y/((-x)/y*(-5.6)+(-x)+(-x))*((-x)+(-x)*(-0.4)+2.7)-((-x)*x*(-3.1)*7.7)+((-y)*(-6.0)))/(-y)-((-3.8)/(-5.3)/y)+((-4.5))-(0.2-y)*((-6.4)*3.3+(-8.3))+8.8)
            wr.tt <| (I 151)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 152
        ctx.emit.comment "test152"
        //let z0 = (6.7*(((-x)+(-y)+y-(-y))*(-x)-((-7.4))*(y))-4.6/((8.2+(-6.0)+y-(-y)/y)-3.8/(-x)-(-x))*x)
        //printfn "%d" <| 152
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((6.7*(((-x)+(-y)+y-(-y))*(-x)-((-7.4))*(y))-4.6/((8.2+(-6.0)+y-(-y)/y)-3.8/(-x)-(-x))*x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (6.7*(((-p)+(-q)+q-(-q))*(-p)-((-7.4))*(q))-4.6/((8.2+(-6.0)+q-(-q)/q)-3.8/(-p)-(-p))*p)
            z2 <== (6.7*(((-x)+(-y)+y-(-y))*(-x)-((-7.4))*(y))-4.6/((8.2+(-6.0)+y-(-y)/y)-3.8/(-x)-(-x))*x)
            wr.tt <| (I 152)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 153
        ctx.emit.comment "test153"
        //let z0 = (-2.7)
        //printfn "%d" <| 153
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 154
        ctx.emit.comment "test154"
        //let z0 = x/(-y)
        //printfn "%d" <| 154
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x/(-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p/(-q)
            z2 <== x/(-y)
            wr.tt <| (I 154)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 155
        ctx.emit.comment "test155"
        //let z0 = ((8.1*((-y)+6.5*(-0.6)+(-8.5)))+(4.8*((-8.6)*(-7.3)/y/8.1+(-3.7))-(-y)+((-3.2)+2.1+7.7-y-(-6.1)))+y)
        //printfn "%d" <| 155
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((8.1*((-y)+6.5*(-0.6)+(-8.5)))+(4.8*((-8.6)*(-7.3)/y/8.1+(-3.7))-(-y)+((-3.2)+2.1+7.7-y-(-6.1)))+y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((8.1*((-q)+6.5*(-0.6)+(-8.5)))+(4.8*((-8.6)*(-7.3)/q/8.1+(-3.7))-(-q)+((-3.2)+2.1+7.7-q-(-6.1)))+q)
            z2 <== ((8.1*((-y)+6.5*(-0.6)+(-8.5)))+(4.8*((-8.6)*(-7.3)/y/8.1+(-3.7))-(-y)+((-3.2)+2.1+7.7-y-(-6.1)))+y)
            wr.tt <| (I 155)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 156
        ctx.emit.comment "test156"
        //let z0 = (0.4+(-6.8)/((x*0.5))+(((-2.3)+(-x)-(-y)+x-(-5.3))/0.4))
        //printfn "%d" <| 156
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((0.4+(-6.8)/((x*0.5))+(((-2.3)+(-x)-(-y)+x-(-5.3))/0.4))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (0.4+(-6.8)/((p*0.5))+(((-2.3)+(-p)-(-q)+p-(-5.3))/0.4))
            z2 <== (0.4+(-6.8)/((x*0.5))+(((-2.3)+(-x)-(-y)+x-(-5.3))/0.4))
            wr.tt <| (I 156)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 157
        ctx.emit.comment "test157"
        //let z0 = 3.2
        //printfn "%d" <| 157
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 158
        ctx.emit.comment "test158"
        //let z0 = ((-8.8))
        //printfn "%d" <| 158
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 159
        ctx.emit.comment "test159"
        //let z0 = y
        //printfn "%d" <| 159
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 159)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 160
        ctx.emit.comment "test160"
        //let z0 = x
        //printfn "%d" <| 160
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 160)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 161
        ctx.emit.comment "test161"
        //let z0 = ((-y)/(0.6*(-0.0)-y+y+(-5.3))-y+(-2.3))+(-x)/((8.1+(-6.6)+(-x)/6.5/(-y)))*y*((y)*(-y)/(-x)/(-2.6)-((-y)*(-2.2)/x))
        //printfn "%d" <| 161
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)/(0.6*(-0.0)-y+y+(-5.3))-y+(-2.3))+(-x)/((8.1+(-6.6)+(-x)/6.5/(-y)))*y*((y)*(-y)/(-x)/(-2.6)-((-y)*(-2.2)/x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)/(0.6*(-0.0)-q+q+(-5.3))-q+(-2.3))+(-p)/((8.1+(-6.6)+(-p)/6.5/(-q)))*q*((q)*(-q)/(-p)/(-2.6)-((-q)*(-2.2)/p))
            z2 <== ((-y)/(0.6*(-0.0)-y+y+(-5.3))-y+(-2.3))+(-x)/((8.1+(-6.6)+(-x)/6.5/(-y)))*y*((y)*(-y)/(-x)/(-2.6)-((-y)*(-2.2)/x))
            wr.tt <| (I 161)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 162
        ctx.emit.comment "test162"
        //let z0 = (-0.5)
        //printfn "%d" <| 162
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 163
        ctx.emit.comment "test163"
        //let z0 = (8.4-(((-x))*(-x)*x+((-y)/y))-(((-y)-(-0.6))/((-y)/(-y)-8.0)/((-y)+(-x))-(x)-(-x))+(x*(-7.1)*x/x/1.8/y-2.6/x))
        //printfn "%d" <| 163
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((8.4-(((-x))*(-x)*x+((-y)/y))-(((-y)-(-0.6))/((-y)/(-y)-8.0)/((-y)+(-x))-(x)-(-x))+(x*(-7.1)*x/x/1.8/y-2.6/x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (8.4-(((-p))*(-p)*p+((-q)/q))-(((-q)-(-0.6))/((-q)/(-q)-8.0)/((-q)+(-p))-(p)-(-p))+(p*(-7.1)*p/p/1.8/q-2.6/p))
            z2 <== (8.4-(((-x))*(-x)*x+((-y)/y))-(((-y)-(-0.6))/((-y)/(-y)-8.0)/((-y)+(-x))-(x)-(-x))+(x*(-7.1)*x/x/1.8/y-2.6/x))
            wr.tt <| (I 163)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 164
        ctx.emit.comment "test164"
        //let z0 = x
        //printfn "%d" <| 164
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 164)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 165
        ctx.emit.comment "test165"
        //let z0 = (-3.2)
        //printfn "%d" <| 165
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 166
        ctx.emit.comment "test166"
        //let z0 = ((-6.7)+(-x)-(((-x)*(-x)+x)+y))
        //printfn "%d" <| 166
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-6.7)+(-x)-(((-x)*(-x)+x)+y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-6.7)+(-p)-(((-p)*(-p)+p)+q))
            z2 <== ((-6.7)+(-x)-(((-x)*(-x)+x)+y))
            wr.tt <| (I 166)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 167
        ctx.emit.comment "test167"
        //let z0 = ((y)+(y+7.5*(-6.8)/(-8.7))+(-4.8)*(x+6.5/0.8-(-x))*3.0)
        //printfn "%d" <| 167
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y)+(y+7.5*(-6.8)/(-8.7))+(-4.8)*(x+6.5/0.8-(-x))*3.0)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q)+(q+7.5*(-6.8)/(-8.7))+(-4.8)*(p+6.5/0.8-(-p))*3.0)
            z2 <== ((y)+(y+7.5*(-6.8)/(-8.7))+(-4.8)*(x+6.5/0.8-(-x))*3.0)
            wr.tt <| (I 167)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 168
        ctx.emit.comment "test168"
        //let z0 = (7.5-x*(-y)*x+(-8.3)/(-y)*y+y)
        //printfn "%d" <| 168
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.5-x*(-y)*x+(-8.3)/(-y)*y+y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.5-p*(-q)*p+(-8.3)/(-q)*q+q)
            z2 <== (7.5-x*(-y)*x+(-8.3)/(-y)*y+y)
            wr.tt <| (I 168)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 169
        ctx.emit.comment "test169"
        //let z0 = y
        //printfn "%d" <| 169
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 169)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 170
        ctx.emit.comment "test170"
        //let z0 = (-0.2)-(-x)+(-x)*(6.3*((-x))+(y*(-x))-(8.3+(-y)/(-y))*(0.8))+(-3.0)
        //printfn "%d" <| 170
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-0.2)-(-x)+(-x)*(6.3*((-x))+(y*(-x))-(8.3+(-y)/(-y))*(0.8))+(-3.0)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-0.2)-(-p)+(-p)*(6.3*((-p))+(q*(-p))-(8.3+(-q)/(-q))*(0.8))+(-3.0)
            z2 <== (-0.2)-(-x)+(-x)*(6.3*((-x))+(y*(-x))-(8.3+(-y)/(-y))*(0.8))+(-3.0)
            wr.tt <| (I 170)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 171
        ctx.emit.comment "test171"
        //let z0 = 8.8
        //printfn "%d" <| 171
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 172
        ctx.emit.comment "test172"
        //let z0 = (((-x)))*((-x)*4.5-((-x)*y*7.4+2.4/(-6.8)))/(-4.2)*2.0/(-y)/0.6+(-y)/(5.6)*(y/(-y)/6.0+0.6)/(-3.2)*(((-4.8)))
        //printfn "%d" <| 172
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)))*((-x)*4.5-((-x)*y*7.4+2.4/(-6.8)))/(-4.2)*2.0/(-y)/0.6+(-y)/(5.6)*(y/(-y)/6.0+0.6)/(-3.2)*(((-4.8)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)))*((-p)*4.5-((-p)*q*7.4+2.4/(-6.8)))/(-4.2)*2.0/(-q)/0.6+(-q)/(5.6)*(q/(-q)/6.0+0.6)/(-3.2)*(((-4.8)))
            z2 <== (((-x)))*((-x)*4.5-((-x)*y*7.4+2.4/(-6.8)))/(-4.2)*2.0/(-y)/0.6+(-y)/(5.6)*(y/(-y)/6.0+0.6)/(-3.2)*(((-4.8)))
            wr.tt <| (I 172)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 173
        ctx.emit.comment "test173"
        //let z0 = (-y)
        //printfn "%d" <| 173
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 173)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 174
        ctx.emit.comment "test174"
        //let z0 = (-7.7)
        //printfn "%d" <| 174
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 175
        ctx.emit.comment "test175"
        //let z0 = ((-8.3))
        //printfn "%d" <| 175
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 176
        ctx.emit.comment "test176"
        //let z0 = ((-y)/(-0.0)/(-x))
        //printfn "%d" <| 176
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)/(-0.0)/(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)/(-0.0)/(-p))
            z2 <== ((-y)/(-0.0)/(-x))
            wr.tt <| (I 176)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 177
        ctx.emit.comment "test177"
        //let z0 = (4.1-(-0.8)*(-y)-(-0.7)/(-8.8))-(-y)-y+x+y*(-x)+(y*3.6/y)+(-y)+((6.1)+(-y)-x-((-y)/(-x)-(-x)-(-8.8))/((-5.5)+(-y)))
        //printfn "%d" <| 177
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((4.1-(-0.8)*(-y)-(-0.7)/(-8.8))-(-y)-y+x+y*(-x)+(y*3.6/y)+(-y)+((6.1)+(-y)-x-((-y)/(-x)-(-x)-(-8.8))/((-5.5)+(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (4.1-(-0.8)*(-q)-(-0.7)/(-8.8))-(-q)-q+p+q*(-p)+(q*3.6/q)+(-q)+((6.1)+(-q)-p-((-q)/(-p)-(-p)-(-8.8))/((-5.5)+(-q)))
            z2 <== (4.1-(-0.8)*(-y)-(-0.7)/(-8.8))-(-y)-y+x+y*(-x)+(y*3.6/y)+(-y)+((6.1)+(-y)-x-((-y)/(-x)-(-x)-(-8.8))/((-5.5)+(-y)))
            wr.tt <| (I 177)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 178
        ctx.emit.comment "test178"
        //let z0 = (2.7/(x/0.4+x+(-y)/x))/6.3*(3.6/(-5.4)-x+2.3*x/3.4+((-y)/(-8.5)+(-7.3))/(-x))*(-x)
        //printfn "%d" <| 178
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((2.7/(x/0.4+x+(-y)/x))/6.3*(3.6/(-5.4)-x+2.3*x/3.4+((-y)/(-8.5)+(-7.3))/(-x))*(-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (2.7/(p/0.4+p+(-q)/p))/6.3*(3.6/(-5.4)-p+2.3*p/3.4+((-q)/(-8.5)+(-7.3))/(-p))*(-p)
            z2 <== (2.7/(x/0.4+x+(-y)/x))/6.3*(3.6/(-5.4)-x+2.3*x/3.4+((-y)/(-8.5)+(-7.3))/(-x))*(-x)
            wr.tt <| (I 178)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 179
        ctx.emit.comment "test179"
        //let z0 = ((y+(-0.2)*(3.4-(-y)+0.1+y)*((-x))/(x+(-x)*(-x)/x))/x)
        //printfn "%d" <| 179
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y+(-0.2)*(3.4-(-y)+0.1+y)*((-x))/(x+(-x)*(-x)/x))/x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q+(-0.2)*(3.4-(-q)+0.1+q)*((-p))/(p+(-p)*(-p)/p))/p)
            z2 <== ((y+(-0.2)*(3.4-(-y)+0.1+y)*((-x))/(x+(-x)*(-x)/x))/x)
            wr.tt <| (I 179)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 180
        ctx.emit.comment "test180"
        //let z0 = 8.0
        //printfn "%d" <| 180
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 181
        ctx.emit.comment "test181"
        //let z0 = ((x+3.6*y*(-x))*((-2.5)/8.7+(-y))-(-0.0)*2.3-(-y)*(-1.0)/x)
        //printfn "%d" <| 181
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x+3.6*y*(-x))*((-2.5)/8.7+(-y))-(-0.0)*2.3-(-y)*(-1.0)/x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p+3.6*q*(-p))*((-2.5)/8.7+(-q))-(-0.0)*2.3-(-q)*(-1.0)/p)
            z2 <== ((x+3.6*y*(-x))*((-2.5)/8.7+(-y))-(-0.0)*2.3-(-y)*(-1.0)/x)
            wr.tt <| (I 181)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 182
        ctx.emit.comment "test182"
        //let z0 = (-3.5)
        //printfn "%d" <| 182
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 183
        ctx.emit.comment "test183"
        //let z0 = (y*((-1.4)-7.0/(-8.7))*((x+(-x)*x*x+(-y))-(-6.7)*((-1.0)+(-x)-(-2.1))*(x))/((8.2*x/x-(-7.6))-((-5.7)/(-y))*y-y+((-y)-(-x)/y)))
        //printfn "%d" <| 183
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y*((-1.4)-7.0/(-8.7))*((x+(-x)*x*x+(-y))-(-6.7)*((-1.0)+(-x)-(-2.1))*(x))/((8.2*x/x-(-7.6))-((-5.7)/(-y))*y-y+((-y)-(-x)/y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q*((-1.4)-7.0/(-8.7))*((p+(-p)*p*p+(-q))-(-6.7)*((-1.0)+(-p)-(-2.1))*(p))/((8.2*p/p-(-7.6))-((-5.7)/(-q))*q-q+((-q)-(-p)/q)))
            z2 <== (y*((-1.4)-7.0/(-8.7))*((x+(-x)*x*x+(-y))-(-6.7)*((-1.0)+(-x)-(-2.1))*(x))/((8.2*x/x-(-7.6))-((-5.7)/(-y))*y-y+((-y)-(-x)/y)))
            wr.tt <| (I 183)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 184
        ctx.emit.comment "test184"
        //let z0 = (-y)
        //printfn "%d" <| 184
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 184)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 185
        ctx.emit.comment "test185"
        //let z0 = (-x)
        //printfn "%d" <| 185
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 185)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 186
        ctx.emit.comment "test186"
        //let z0 = (-4.8)*y/((3.0)*(-8.3)+(y+x+(-x)-8.1))
        //printfn "%d" <| 186
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-4.8)*y/((3.0)*(-8.3)+(y+x+(-x)-8.1))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-4.8)*q/((3.0)*(-8.3)+(q+p+(-p)-8.1))
            z2 <== (-4.8)*y/((3.0)*(-8.3)+(y+x+(-x)-8.1))
            wr.tt <| (I 186)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 187
        ctx.emit.comment "test187"
        //let z0 = (-8.2)
        //printfn "%d" <| 187
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 188
        ctx.emit.comment "test188"
        //let z0 = (-x)
        //printfn "%d" <| 188
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 188)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 189
        ctx.emit.comment "test189"
        //let z0 = (-y)
        //printfn "%d" <| 189
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 189)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 190
        ctx.emit.comment "test190"
        //let z0 = (y+((y+y-x*x*(-y))-(-x)+((-0.8)/(-x)/5.0*2.0*3.7)/(-4.0)))
        //printfn "%d" <| 190
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+((y+y-x*x*(-y))-(-x)+((-0.8)/(-x)/5.0*2.0*3.7)/(-4.0)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+((q+q-p*p*(-q))-(-p)+((-0.8)/(-p)/5.0*2.0*3.7)/(-4.0)))
            z2 <== (y+((y+y-x*x*(-y))-(-x)+((-0.8)/(-x)/5.0*2.0*3.7)/(-4.0)))
            wr.tt <| (I 190)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 191
        ctx.emit.comment "test191"
        //let z0 = x
        //printfn "%d" <| 191
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 191)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 192
        ctx.emit.comment "test192"
        //let z0 = (((7.6/0.1-(-y)*y)/5.4+((-5.5)))*(5.5-(0.7*y+8.3)*((-8.1))-(-x)*(-2.1)-((-8.4)*(-8.5)*(-8.0)))-(((-7.3)*8.8*(-x)/(-6.2))))
        //printfn "%d" <| 192
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((7.6/0.1-(-y)*y)/5.4+((-5.5)))*(5.5-(0.7*y+8.3)*((-8.1))-(-x)*(-2.1)-((-8.4)*(-8.5)*(-8.0)))-(((-7.3)*8.8*(-x)/(-6.2))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((7.6/0.1-(-q)*q)/5.4+((-5.5)))*(5.5-(0.7*q+8.3)*((-8.1))-(-p)*(-2.1)-((-8.4)*(-8.5)*(-8.0)))-(((-7.3)*8.8*(-p)/(-6.2))))
            z2 <== (((7.6/0.1-(-y)*y)/5.4+((-5.5)))*(5.5-(0.7*y+8.3)*((-8.1))-(-x)*(-2.1)-((-8.4)*(-8.5)*(-8.0)))-(((-7.3)*8.8*(-x)/(-6.2))))
            wr.tt <| (I 192)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 193
        ctx.emit.comment "test193"
        //let z0 = 3.2
        //printfn "%d" <| 193
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 194
        ctx.emit.comment "test194"
        //let z0 = (-8.5)
        //printfn "%d" <| 194
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 195
        ctx.emit.comment "test195"
        //let z0 = ((-7.8))
        //printfn "%d" <| 195
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 196
        ctx.emit.comment "test196"
        //let z0 = (-x)
        //printfn "%d" <| 196
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 196)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 197
        ctx.emit.comment "test197"
        //let z0 = (0.6-((-6.3)*(5.0+(-6.7))+x*x/((-x)-(-x)/3.6+(-y)))+(-4.4)/((-7.4)/3.2+((-y)*(-4.5)/(-0.2)-x+(-y))))
        //printfn "%d" <| 197
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((0.6-((-6.3)*(5.0+(-6.7))+x*x/((-x)-(-x)/3.6+(-y)))+(-4.4)/((-7.4)/3.2+((-y)*(-4.5)/(-0.2)-x+(-y))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (0.6-((-6.3)*(5.0+(-6.7))+p*p/((-p)-(-p)/3.6+(-q)))+(-4.4)/((-7.4)/3.2+((-q)*(-4.5)/(-0.2)-p+(-q))))
            z2 <== (0.6-((-6.3)*(5.0+(-6.7))+x*x/((-x)-(-x)/3.6+(-y)))+(-4.4)/((-7.4)/3.2+((-y)*(-4.5)/(-0.2)-x+(-y))))
            wr.tt <| (I 197)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 198
        ctx.emit.comment "test198"
        //let z0 = 2.5
        //printfn "%d" <| 198
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 199
        ctx.emit.comment "test199"
        //let z0 = ((y-((-x)*(-y)))/(-x)-3.4-0.1)
        //printfn "%d" <| 199
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y-((-x)*(-y)))/(-x)-3.4-0.1)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q-((-p)*(-q)))/(-p)-3.4-0.1)
            z2 <== ((y-((-x)*(-y)))/(-x)-3.4-0.1)
            wr.tt <| (I 199)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 200
        ctx.emit.comment "test200"
        //let z0 = (-8.0)
        //printfn "%d" <| 200
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 201
        ctx.emit.comment "test201"
        //let z0 = (y+y*(((-y)+(-6.2)+(-2.7)*(-y)*(-1.2)))*((7.0/(-3.4))*y*((-4.4)/(-2.6)/(-x)+2.0*y)))
        //printfn "%d" <| 201
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+y*(((-y)+(-6.2)+(-2.7)*(-y)*(-1.2)))*((7.0/(-3.4))*y*((-4.4)/(-2.6)/(-x)+2.0*y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+q*(((-q)+(-6.2)+(-2.7)*(-q)*(-1.2)))*((7.0/(-3.4))*q*((-4.4)/(-2.6)/(-p)+2.0*q)))
            z2 <== (y+y*(((-y)+(-6.2)+(-2.7)*(-y)*(-1.2)))*((7.0/(-3.4))*y*((-4.4)/(-2.6)/(-x)+2.0*y)))
            wr.tt <| (I 201)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 202
        ctx.emit.comment "test202"
        //let z0 = ((7.0/7.1*y+(-x)*x+(-8.2)*(-x))+7.2+(((-y))*(y-(-y)*(-x)+2.4)-(-y)/3.2-x)+x+4.1+y)
        //printfn "%d" <| 202
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((7.0/7.1*y+(-x)*x+(-8.2)*(-x))+7.2+(((-y))*(y-(-y)*(-x)+2.4)-(-y)/3.2-x)+x+4.1+y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((7.0/7.1*q+(-p)*p+(-8.2)*(-p))+7.2+(((-q))*(q-(-q)*(-p)+2.4)-(-q)/3.2-p)+p+4.1+q)
            z2 <== ((7.0/7.1*y+(-x)*x+(-8.2)*(-x))+7.2+(((-y))*(y-(-y)*(-x)+2.4)-(-y)/3.2-x)+x+4.1+y)
            wr.tt <| (I 202)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 203
        ctx.emit.comment "test203"
        //let z0 = (-7.2)
        //printfn "%d" <| 203
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 204
        ctx.emit.comment "test204"
        //let z0 = (-x)
        //printfn "%d" <| 204
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 204)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 205
        ctx.emit.comment "test205"
        //let z0 = 0.4
        //printfn "%d" <| 205
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 206
        ctx.emit.comment "test206"
        //let z0 = (-0.3)
        //printfn "%d" <| 206
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 207
        ctx.emit.comment "test207"
        //let z0 = 4.5
        //printfn "%d" <| 207
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 208
        ctx.emit.comment "test208"
        //let z0 = ((((-y)-(-x)+(-x))+(x))/(1.4)+(6.7*x+(-8.3)/(-7.2)/(-0.1))+3.8-2.1+6.0+(((-y)-(-6.1)*(-8.1)/y+(-x))+(4.1/(-y)*5.4-x-(-4.7))/(y*(-1.0))+x)-((2.3+(-y)/(-y)*2.8)+2.5-(-8.7)*((-4.4)-0.3+2.8+7.0-6.0)-8.0))
        //printfn "%d" <| 208
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-y)-(-x)+(-x))+(x))/(1.4)+(6.7*x+(-8.3)/(-7.2)/(-0.1))+3.8-2.1+6.0+(((-y)-(-6.1)*(-8.1)/y+(-x))+(4.1/(-y)*5.4-x-(-4.7))/(y*(-1.0))+x)-((2.3+(-y)/(-y)*2.8)+2.5-(-8.7)*((-4.4)-0.3+2.8+7.0-6.0)-8.0))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-q)-(-p)+(-p))+(p))/(1.4)+(6.7*p+(-8.3)/(-7.2)/(-0.1))+3.8-2.1+6.0+(((-q)-(-6.1)*(-8.1)/q+(-p))+(4.1/(-q)*5.4-p-(-4.7))/(q*(-1.0))+p)-((2.3+(-q)/(-q)*2.8)+2.5-(-8.7)*((-4.4)-0.3+2.8+7.0-6.0)-8.0))
            z2 <== ((((-y)-(-x)+(-x))+(x))/(1.4)+(6.7*x+(-8.3)/(-7.2)/(-0.1))+3.8-2.1+6.0+(((-y)-(-6.1)*(-8.1)/y+(-x))+(4.1/(-y)*5.4-x-(-4.7))/(y*(-1.0))+x)-((2.3+(-y)/(-y)*2.8)+2.5-(-8.7)*((-4.4)-0.3+2.8+7.0-6.0)-8.0))
            wr.tt <| (I 208)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 209
        ctx.emit.comment "test209"
        //let z0 = (-3.8)
        //printfn "%d" <| 209
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 210
        ctx.emit.comment "test210"
        //let z0 = (-x)
        //printfn "%d" <| 210
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 210)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 211
        ctx.emit.comment "test211"
        //let z0 = (((-4.3)/x-7.8+6.4/y)/(-7.4))
        //printfn "%d" <| 211
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-4.3)/x-7.8+6.4/y)/(-7.4))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-4.3)/p-7.8+6.4/q)/(-7.4))
            z2 <== (((-4.3)/x-7.8+6.4/y)/(-7.4))
            wr.tt <| (I 211)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 212
        ctx.emit.comment "test212"
        //let z0 = 5.4
        //printfn "%d" <| 212
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 213
        ctx.emit.comment "test213"
        //let z0 = (7.1-(-x)-x)
        //printfn "%d" <| 213
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.1-(-x)-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.1-(-p)-p)
            z2 <== (7.1-(-x)-x)
            wr.tt <| (I 213)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 214
        ctx.emit.comment "test214"
        //let z0 = (3.8*4.7*(((-6.5)/(-x))*((-5.1)-x*(-x))*(x-6.1*(-4.4)/(-6.0)+(-7.2))/(-6.4)*(-x))/y)
        //printfn "%d" <| 214
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((3.8*4.7*(((-6.5)/(-x))*((-5.1)-x*(-x))*(x-6.1*(-4.4)/(-6.0)+(-7.2))/(-6.4)*(-x))/y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (3.8*4.7*(((-6.5)/(-p))*((-5.1)-p*(-p))*(p-6.1*(-4.4)/(-6.0)+(-7.2))/(-6.4)*(-p))/q)
            z2 <== (3.8*4.7*(((-6.5)/(-x))*((-5.1)-x*(-x))*(x-6.1*(-4.4)/(-6.0)+(-7.2))/(-6.4)*(-x))/y)
            wr.tt <| (I 214)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 215
        ctx.emit.comment "test215"
        //let z0 = 6.7
        //printfn "%d" <| 215
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 216
        ctx.emit.comment "test216"
        //let z0 = (-7.2)
        //printfn "%d" <| 216
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 217
        ctx.emit.comment "test217"
        //let z0 = 5.5
        //printfn "%d" <| 217
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 218
        ctx.emit.comment "test218"
        //let z0 = (8.4+((-y)))
        //printfn "%d" <| 218
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((8.4+((-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (8.4+((-q)))
            z2 <== (8.4+((-y)))
            wr.tt <| (I 218)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 219
        ctx.emit.comment "test219"
        //let z0 = (8.2-(-8.3)*8.2/y)*x+(8.0+((-y)/(-x)/y*(-6.7)-x))
        //printfn "%d" <| 219
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((8.2-(-8.3)*8.2/y)*x+(8.0+((-y)/(-x)/y*(-6.7)-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (8.2-(-8.3)*8.2/q)*p+(8.0+((-q)/(-p)/q*(-6.7)-p))
            z2 <== (8.2-(-8.3)*8.2/y)*x+(8.0+((-y)/(-x)/y*(-6.7)-x))
            wr.tt <| (I 219)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 220
        ctx.emit.comment "test220"
        //let z0 = ((x)+x*x*((-x)*x/(-5.8)-(-6.5))-((-8.3)+(-6.2)/(-x)))
        //printfn "%d" <| 220
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x)+x*x*((-x)*x/(-5.8)-(-6.5))-((-8.3)+(-6.2)/(-x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p)+p*p*((-p)*p/(-5.8)-(-6.5))-((-8.3)+(-6.2)/(-p)))
            z2 <== ((x)+x*x*((-x)*x/(-5.8)-(-6.5))-((-8.3)+(-6.2)/(-x)))
            wr.tt <| (I 220)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 221
        ctx.emit.comment "test221"
        //let z0 = (8.3*((x)))
        //printfn "%d" <| 221
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((8.3*((x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (8.3*((p)))
            z2 <== (8.3*((x)))
            wr.tt <| (I 221)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 222
        ctx.emit.comment "test222"
        //let z0 = ((-y)+((y+y+4.0+(-2.1)-(-5.7))/(6.1/(-0.7)/y+(-5.1))/(-0.6))-x)
        //printfn "%d" <| 222
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)+((y+y+4.0+(-2.1)-(-5.7))/(6.1/(-0.7)/y+(-5.1))/(-0.6))-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)+((q+q+4.0+(-2.1)-(-5.7))/(6.1/(-0.7)/q+(-5.1))/(-0.6))-p)
            z2 <== ((-y)+((y+y+4.0+(-2.1)-(-5.7))/(6.1/(-0.7)/y+(-5.1))/(-0.6))-x)
            wr.tt <| (I 222)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 223
        ctx.emit.comment "test223"
        //let z0 = (5.7)
        //printfn "%d" <| 223
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 224
        ctx.emit.comment "test224"
        //let z0 = 2.5
        //printfn "%d" <| 224
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 225
        ctx.emit.comment "test225"
        //let z0 = (x*x)
        //printfn "%d" <| 225
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x*x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p*p)
            z2 <== (x*x)
            wr.tt <| (I 225)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 226
        ctx.emit.comment "test226"
        //let z0 = (-2.0)
        //printfn "%d" <| 226
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 227
        ctx.emit.comment "test227"
        //let z0 = (-3.6)
        //printfn "%d" <| 227
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 228
        ctx.emit.comment "test228"
        //let z0 = 4.1
        //printfn "%d" <| 228
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 229
        ctx.emit.comment "test229"
        //let z0 = (((-7.7)+(-3.1)/7.2)+(-y))
        //printfn "%d" <| 229
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-7.7)+(-3.1)/7.2)+(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-7.7)+(-3.1)/7.2)+(-q))
            z2 <== (((-7.7)+(-3.1)/7.2)+(-y))
            wr.tt <| (I 229)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 230
        ctx.emit.comment "test230"
        //let z0 = (-y)
        //printfn "%d" <| 230
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 230)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 231
        ctx.emit.comment "test231"
        //let z0 = 2.7
        //printfn "%d" <| 231
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 232
        ctx.emit.comment "test232"
        //let z0 = (-y)
        //printfn "%d" <| 232
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 232)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 233
        ctx.emit.comment "test233"
        //let z0 = 4.8
        //printfn "%d" <| 233
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 234
        ctx.emit.comment "test234"
        //let z0 = (-7.6)
        //printfn "%d" <| 234
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 235
        ctx.emit.comment "test235"
        //let z0 = (-3.7)
        //printfn "%d" <| 235
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 236
        ctx.emit.comment "test236"
        //let z0 = y
        //printfn "%d" <| 236
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 236)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 237
        ctx.emit.comment "test237"
        //let z0 = (x*((x/(-y)-2.6/(-y)+y)/y))
        //printfn "%d" <| 237
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x*((x/(-y)-2.6/(-y)+y)/y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p*((p/(-q)-2.6/(-q)+q)/q))
            z2 <== (x*((x/(-y)-2.6/(-y)+y)/y))
            wr.tt <| (I 237)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 238
        ctx.emit.comment "test238"
        //let z0 = ((-4.4)/((y*(-x)*y)/x+x+y*(-y)/(-x)/(-0.0))/(-1.2)*(((-5.6)/6.6)/((-x)-x/8.8)/x/5.8-x)+(((-y)-x/4.8)*((-x)*x)))
        //printfn "%d" <| 238
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-4.4)/((y*(-x)*y)/x+x+y*(-y)/(-x)/(-0.0))/(-1.2)*(((-5.6)/6.6)/((-x)-x/8.8)/x/5.8-x)+(((-y)-x/4.8)*((-x)*x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-4.4)/((q*(-p)*q)/p+p+q*(-q)/(-p)/(-0.0))/(-1.2)*(((-5.6)/6.6)/((-p)-p/8.8)/p/5.8-p)+(((-q)-p/4.8)*((-p)*p)))
            z2 <== ((-4.4)/((y*(-x)*y)/x+x+y*(-y)/(-x)/(-0.0))/(-1.2)*(((-5.6)/6.6)/((-x)-x/8.8)/x/5.8-x)+(((-y)-x/4.8)*((-x)*x)))
            wr.tt <| (I 238)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 239
        ctx.emit.comment "test239"
        //let z0 = (0.6)
        //printfn "%d" <| 239
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 240
        ctx.emit.comment "test240"
        //let z0 = ((y)+((-6.5)+(-x)*(-y))*((x/(-7.7)/y*(-x)+(-x))+x/6.8)+(-y))
        //printfn "%d" <| 240
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y)+((-6.5)+(-x)*(-y))*((x/(-7.7)/y*(-x)+(-x))+x/6.8)+(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q)+((-6.5)+(-p)*(-q))*((p/(-7.7)/q*(-p)+(-p))+p/6.8)+(-q))
            z2 <== ((y)+((-6.5)+(-x)*(-y))*((x/(-7.7)/y*(-x)+(-x))+x/6.8)+(-y))
            wr.tt <| (I 240)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 241
        ctx.emit.comment "test241"
        //let z0 = 6.2
        //printfn "%d" <| 241
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 242
        ctx.emit.comment "test242"
        //let z0 = (y*2.4)
        //printfn "%d" <| 242
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y*2.4)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q*2.4)
            z2 <== (y*2.4)
            wr.tt <| (I 242)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 243
        ctx.emit.comment "test243"
        //let z0 = y*((x)-(x)-(-6.1))-((-x))-y
        //printfn "%d" <| 243
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y*((x)-(x)-(-6.1))-((-x))-y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q*((p)-(p)-(-6.1))-((-p))-q
            z2 <== y*((x)-(x)-(-6.1))-((-x))-y
            wr.tt <| (I 243)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 244
        ctx.emit.comment "test244"
        //let z0 = (-x)
        //printfn "%d" <| 244
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 244)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 245
        ctx.emit.comment "test245"
        //let z0 = (((-6.1)/(-0.1)/y)+x*(-7.6)*8.2*0.1)-(-y)
        //printfn "%d" <| 245
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-6.1)/(-0.1)/y)+x*(-7.6)*8.2*0.1)-(-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-6.1)/(-0.1)/q)+p*(-7.6)*8.2*0.1)-(-q)
            z2 <== (((-6.1)/(-0.1)/y)+x*(-7.6)*8.2*0.1)-(-y)
            wr.tt <| (I 245)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 246
        ctx.emit.comment "test246"
        //let z0 = ((-x)/(-y))
        //printfn "%d" <| 246
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)/(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)/(-q))
            z2 <== ((-x)/(-y))
            wr.tt <| (I 246)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 247
        ctx.emit.comment "test247"
        //let z0 = 1.5+4.0
        //printfn "%d" <| 247
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 248
        ctx.emit.comment "test248"
        //let z0 = ((0.5/((-8.7)/(-y)/6.4+0.8/0.4))*(-x)/(-x))
        //printfn "%d" <| 248
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((0.5/((-8.7)/(-y)/6.4+0.8/0.4))*(-x)/(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((0.5/((-8.7)/(-q)/6.4+0.8/0.4))*(-p)/(-p))
            z2 <== ((0.5/((-8.7)/(-y)/6.4+0.8/0.4))*(-x)/(-x))
            wr.tt <| (I 248)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 249
        ctx.emit.comment "test249"
        //let z0 = (x+((-7.0)+(0.7+(-3.0)/(-2.7)/(-y)-(-y))+((-0.3)/5.8)/6.5-x)*8.0)
        //printfn "%d" <| 249
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x+((-7.0)+(0.7+(-3.0)/(-2.7)/(-y)-(-y))+((-0.3)/5.8)/6.5-x)*8.0)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p+((-7.0)+(0.7+(-3.0)/(-2.7)/(-q)-(-q))+((-0.3)/5.8)/6.5-p)*8.0)
            z2 <== (x+((-7.0)+(0.7+(-3.0)/(-2.7)/(-y)-(-y))+((-0.3)/5.8)/6.5-x)*8.0)
            wr.tt <| (I 249)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 250
        ctx.emit.comment "test250"
        //let z0 = (-8.7)
        //printfn "%d" <| 250
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 251
        ctx.emit.comment "test251"
        //let z0 = (((-2.3)/((-6.8)-(-y)+2.5+y)+6.7*(5.1+4.1-(-5.0)))*((-7.3)+y+3.4/(-x)+5.4-2.0*(-y)*3.2/y)+((-x)-x+8.6)-y)
        //printfn "%d" <| 251
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-2.3)/((-6.8)-(-y)+2.5+y)+6.7*(5.1+4.1-(-5.0)))*((-7.3)+y+3.4/(-x)+5.4-2.0*(-y)*3.2/y)+((-x)-x+8.6)-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-2.3)/((-6.8)-(-q)+2.5+q)+6.7*(5.1+4.1-(-5.0)))*((-7.3)+q+3.4/(-p)+5.4-2.0*(-q)*3.2/q)+((-p)-p+8.6)-q)
            z2 <== (((-2.3)/((-6.8)-(-y)+2.5+y)+6.7*(5.1+4.1-(-5.0)))*((-7.3)+y+3.4/(-x)+5.4-2.0*(-y)*3.2/y)+((-x)-x+8.6)-y)
            wr.tt <| (I 251)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 252
        ctx.emit.comment "test252"
        //let z0 = ((3.3)+(((-x)-(-6.3)))/(-5.5))
        //printfn "%d" <| 252
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((3.3)+(((-x)-(-6.3)))/(-5.5))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((3.3)+(((-p)-(-6.3)))/(-5.5))
            z2 <== ((3.3)+(((-x)-(-6.3)))/(-5.5))
            wr.tt <| (I 252)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 253
        ctx.emit.comment "test253"
        //let z0 = ((4.4-2.2-(-5.8)-7.0)-((-1.1)*(-y)-(-x)/(-3.8)/(-x))-6.4+(-7.2))/((5.2+3.6/(-4.2)*8.5/(-0.5)))+(-7.0)-(-5.2)+(y)+((-y))-((-y)-(-4.6)*x-(-x)-(-2.0))+(-0.6)
        //printfn "%d" <| 253
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((4.4-2.2-(-5.8)-7.0)-((-1.1)*(-y)-(-x)/(-3.8)/(-x))-6.4+(-7.2))/((5.2+3.6/(-4.2)*8.5/(-0.5)))+(-7.0)-(-5.2)+(y)+((-y))-((-y)-(-4.6)*x-(-x)-(-2.0))+(-0.6)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((4.4-2.2-(-5.8)-7.0)-((-1.1)*(-q)-(-p)/(-3.8)/(-p))-6.4+(-7.2))/((5.2+3.6/(-4.2)*8.5/(-0.5)))+(-7.0)-(-5.2)+(q)+((-q))-((-q)-(-4.6)*p-(-p)-(-2.0))+(-0.6)
            z2 <== ((4.4-2.2-(-5.8)-7.0)-((-1.1)*(-y)-(-x)/(-3.8)/(-x))-6.4+(-7.2))/((5.2+3.6/(-4.2)*8.5/(-0.5)))+(-7.0)-(-5.2)+(y)+((-y))-((-y)-(-4.6)*x-(-x)-(-2.0))+(-0.6)
            wr.tt <| (I 253)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 254
        ctx.emit.comment "test254"
        //let z0 = (-y)
        //printfn "%d" <| 254
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 254)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 255
        ctx.emit.comment "test255"
        //let z0 = ((-x)+((-0.6)-8.6)+((-x)*y*(-4.2)*(-2.7))+1.8)+((x-(-y)+(-1.6)*6.0)+x*((-x)-0.5*(-6.6)))/(-5.5)
        //printfn "%d" <| 255
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+((-0.6)-8.6)+((-x)*y*(-4.2)*(-2.7))+1.8)+((x-(-y)+(-1.6)*6.0)+x*((-x)-0.5*(-6.6)))/(-5.5)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+((-0.6)-8.6)+((-p)*q*(-4.2)*(-2.7))+1.8)+((p-(-q)+(-1.6)*6.0)+p*((-p)-0.5*(-6.6)))/(-5.5)
            z2 <== ((-x)+((-0.6)-8.6)+((-x)*y*(-4.2)*(-2.7))+1.8)+((x-(-y)+(-1.6)*6.0)+x*((-x)-0.5*(-6.6)))/(-5.5)
            wr.tt <| (I 255)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 256
        ctx.emit.comment "test256"
        //let z0 = y
        //printfn "%d" <| 256
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 256)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 257
        ctx.emit.comment "test257"
        //let z0 = 3.0/((-y)*(-0.1)/(-4.2)+(-y)/1.7)
        //printfn "%d" <| 257
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (3.0/((-y)*(-0.1)/(-4.2)+(-y)/1.7)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 3.0/((-q)*(-0.1)/(-4.2)+(-q)/1.7)
            z2 <== 3.0/((-y)*(-0.1)/(-4.2)+(-y)/1.7)
            wr.tt <| (I 257)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 258
        ctx.emit.comment "test258"
        //let z0 = ((((-4.7)*(-x)))/(-1.8))
        //printfn "%d" <| 258
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-4.7)*(-x)))/(-1.8))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-4.7)*(-p)))/(-1.8))
            z2 <== ((((-4.7)*(-x)))/(-1.8))
            wr.tt <| (I 258)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 259
        ctx.emit.comment "test259"
        //let z0 = ((x+1.4+(-6.3))+(-2.4)+((-y)/8.2/x*0.0*y)-(-4.4)+(-x))
        //printfn "%d" <| 259
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x+1.4+(-6.3))+(-2.4)+((-y)/8.2/x*0.0*y)-(-4.4)+(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p+1.4+(-6.3))+(-2.4)+((-q)/8.2/p*0.0*q)-(-4.4)+(-p))
            z2 <== ((x+1.4+(-6.3))+(-2.4)+((-y)/8.2/x*0.0*y)-(-4.4)+(-x))
            wr.tt <| (I 259)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 260
        ctx.emit.comment "test260"
        //let z0 = y+(-x)*((1.0/x+1.0-(-y)*y)+(-2.2)+((-y)/(-1.3)+(-y))-(1.6*0.2-x+(-y)+y))
        //printfn "%d" <| 260
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y+(-x)*((1.0/x+1.0-(-y)*y)+(-2.2)+((-y)/(-1.3)+(-y))-(1.6*0.2-x+(-y)+y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q+(-p)*((1.0/p+1.0-(-q)*q)+(-2.2)+((-q)/(-1.3)+(-q))-(1.6*0.2-p+(-q)+q))
            z2 <== y+(-x)*((1.0/x+1.0-(-y)*y)+(-2.2)+((-y)/(-1.3)+(-y))-(1.6*0.2-x+(-y)+y))
            wr.tt <| (I 260)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 261
        ctx.emit.comment "test261"
        //let z0 = y
        //printfn "%d" <| 261
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 261)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 262
        ctx.emit.comment "test262"
        //let z0 = (-y)
        //printfn "%d" <| 262
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 262)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 263
        ctx.emit.comment "test263"
        //let z0 = (((-7.6)*(x/x-(-y)*(-x))-(-2.0)+(-y)+(-y))+(-y)-(-7.0)/(-2.6)+(-3.6))
        //printfn "%d" <| 263
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-7.6)*(x/x-(-y)*(-x))-(-2.0)+(-y)+(-y))+(-y)-(-7.0)/(-2.6)+(-3.6))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-7.6)*(p/p-(-q)*(-p))-(-2.0)+(-q)+(-q))+(-q)-(-7.0)/(-2.6)+(-3.6))
            z2 <== (((-7.6)*(x/x-(-y)*(-x))-(-2.0)+(-y)+(-y))+(-y)-(-7.0)/(-2.6)+(-3.6))
            wr.tt <| (I 263)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 264
        ctx.emit.comment "test264"
        //let z0 = 8.6
        //printfn "%d" <| 264
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 265
        ctx.emit.comment "test265"
        //let z0 = 2.5
        //printfn "%d" <| 265
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 266
        ctx.emit.comment "test266"
        //let z0 = ((((-4.4)-1.5-x)*0.8)/y/0.7)
        //printfn "%d" <| 266
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-4.4)-1.5-x)*0.8)/y/0.7)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-4.4)-1.5-p)*0.8)/q/0.7)
            z2 <== ((((-4.4)-1.5-x)*0.8)/y/0.7)
            wr.tt <| (I 266)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 267
        ctx.emit.comment "test267"
        //let z0 = (-7.1)
        //printfn "%d" <| 267
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 268
        ctx.emit.comment "test268"
        //let z0 = (3.1/(2.6*(-y)/1.2)+(-8.7)*(-y))
        //printfn "%d" <| 268
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((3.1/(2.6*(-y)/1.2)+(-8.7)*(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (3.1/(2.6*(-q)/1.2)+(-8.7)*(-q))
            z2 <== (3.1/(2.6*(-y)/1.2)+(-8.7)*(-y))
            wr.tt <| (I 268)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 269
        ctx.emit.comment "test269"
        //let z0 = (((x+(-y)+(-y)/x)+x*(-y))/((y*(-3.3))-((-0.0)*(-x)/(-1.0)+x-8.3)*((-x))*(-y)+((-4.1)+0.4*(-2.8)))/((x/(-x)+(-x)*0.4*(-x))-y*(-x))-((y+(-7.0))*y+(4.7)-(-3.4)-(-y)))
        //printfn "%d" <| 269
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((x+(-y)+(-y)/x)+x*(-y))/((y*(-3.3))-((-0.0)*(-x)/(-1.0)+x-8.3)*((-x))*(-y)+((-4.1)+0.4*(-2.8)))/((x/(-x)+(-x)*0.4*(-x))-y*(-x))-((y+(-7.0))*y+(4.7)-(-3.4)-(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((p+(-q)+(-q)/p)+p*(-q))/((q*(-3.3))-((-0.0)*(-p)/(-1.0)+p-8.3)*((-p))*(-q)+((-4.1)+0.4*(-2.8)))/((p/(-p)+(-p)*0.4*(-p))-q*(-p))-((q+(-7.0))*q+(4.7)-(-3.4)-(-q)))
            z2 <== (((x+(-y)+(-y)/x)+x*(-y))/((y*(-3.3))-((-0.0)*(-x)/(-1.0)+x-8.3)*((-x))*(-y)+((-4.1)+0.4*(-2.8)))/((x/(-x)+(-x)*0.4*(-x))-y*(-x))-((y+(-7.0))*y+(4.7)-(-3.4)-(-y)))
            wr.tt <| (I 269)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 270
        ctx.emit.comment "test270"
        //let z0 = ((-x)/(0.3+0.6+x-x)-8.2/x+y)
        //printfn "%d" <| 270
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)/(0.3+0.6+x-x)-8.2/x+y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)/(0.3+0.6+p-p)-8.2/p+q)
            z2 <== ((-x)/(0.3+0.6+x-x)-8.2/x+y)
            wr.tt <| (I 270)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 271
        ctx.emit.comment "test271"
        //let z0 = ((y/((-4.1))-(y)+4.7*((-2.1))))
        //printfn "%d" <| 271
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y/((-4.1))-(y)+4.7*((-2.1))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q/((-4.1))-(q)+4.7*((-2.1))))
            z2 <== ((y/((-4.1))-(y)+4.7*((-2.1))))
            wr.tt <| (I 271)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 272
        ctx.emit.comment "test272"
        //let z0 = ((((-3.8))-(0.8+2.5+(-y)-(-y)*0.0)*y+(-x)+(5.5+x-2.4/(-y)-8.8))/4.8/((-x)/8.5-y)+(-7.6))
        //printfn "%d" <| 272
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-3.8))-(0.8+2.5+(-y)-(-y)*0.0)*y+(-x)+(5.5+x-2.4/(-y)-8.8))/4.8/((-x)/8.5-y)+(-7.6))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-3.8))-(0.8+2.5+(-q)-(-q)*0.0)*q+(-p)+(5.5+p-2.4/(-q)-8.8))/4.8/((-p)/8.5-q)+(-7.6))
            z2 <== ((((-3.8))-(0.8+2.5+(-y)-(-y)*0.0)*y+(-x)+(5.5+x-2.4/(-y)-8.8))/4.8/((-x)/8.5-y)+(-7.6))
            wr.tt <| (I 272)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 273
        ctx.emit.comment "test273"
        //let z0 = (y-y/(-x)+((-x)*(-x)*(-0.5))+((-x))-1.7)
        //printfn "%d" <| 273
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y-y/(-x)+((-x)*(-x)*(-0.5))+((-x))-1.7)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q-q/(-p)+((-p)*(-p)*(-0.5))+((-p))-1.7)
            z2 <== (y-y/(-x)+((-x)*(-x)*(-0.5))+((-x))-1.7)
            wr.tt <| (I 273)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 274
        ctx.emit.comment "test274"
        //let z0 = ((y/(-6.4)-(-0.0))*((-x)*(-3.2)*(-8.5)/y)-(-y)-(-x)*(y+3.2-x*(-4.6)/y)-(-2.2)/(-y)/4.0)
        //printfn "%d" <| 274
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y/(-6.4)-(-0.0))*((-x)*(-3.2)*(-8.5)/y)-(-y)-(-x)*(y+3.2-x*(-4.6)/y)-(-2.2)/(-y)/4.0)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q/(-6.4)-(-0.0))*((-p)*(-3.2)*(-8.5)/q)-(-q)-(-p)*(q+3.2-p*(-4.6)/q)-(-2.2)/(-q)/4.0)
            z2 <== ((y/(-6.4)-(-0.0))*((-x)*(-3.2)*(-8.5)/y)-(-y)-(-x)*(y+3.2-x*(-4.6)/y)-(-2.2)/(-y)/4.0)
            wr.tt <| (I 274)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 275
        ctx.emit.comment "test275"
        //let z0 = ((-2.5)+(((-7.1))-(6.7-(-x)*(-6.1)+8.1-6.1)-5.2*y-(0.4+y+3.5+0.6+5.5))-(-2.8)+(x)+((-x)/((-2.6)/x-(-0.3)*0.4)+((-5.3)/(-x)-(-0.0)/(-0.1))+y-x))
        //printfn "%d" <| 275
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-2.5)+(((-7.1))-(6.7-(-x)*(-6.1)+8.1-6.1)-5.2*y-(0.4+y+3.5+0.6+5.5))-(-2.8)+(x)+((-x)/((-2.6)/x-(-0.3)*0.4)+((-5.3)/(-x)-(-0.0)/(-0.1))+y-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-2.5)+(((-7.1))-(6.7-(-p)*(-6.1)+8.1-6.1)-5.2*q-(0.4+q+3.5+0.6+5.5))-(-2.8)+(p)+((-p)/((-2.6)/p-(-0.3)*0.4)+((-5.3)/(-p)-(-0.0)/(-0.1))+q-p))
            z2 <== ((-2.5)+(((-7.1))-(6.7-(-x)*(-6.1)+8.1-6.1)-5.2*y-(0.4+y+3.5+0.6+5.5))-(-2.8)+(x)+((-x)/((-2.6)/x-(-0.3)*0.4)+((-5.3)/(-x)-(-0.0)/(-0.1))+y-x))
            wr.tt <| (I 275)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 276
        ctx.emit.comment "test276"
        //let z0 = (-5.8)
        //printfn "%d" <| 276
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 277
        ctx.emit.comment "test277"
        //let z0 = ((-y)-(8.2-(7.8)))
        //printfn "%d" <| 277
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)-(8.2-(7.8)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)-(8.2-(7.8)))
            z2 <== ((-y)-(8.2-(7.8)))
            wr.tt <| (I 277)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 278
        ctx.emit.comment "test278"
        //let z0 = x+x-(-x)
        //printfn "%d" <| 278
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x+x-(-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p+p-(-p)
            z2 <== x+x-(-x)
            wr.tt <| (I 278)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 279
        ctx.emit.comment "test279"
        //let z0 = 5.4
        //printfn "%d" <| 279
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 280
        ctx.emit.comment "test280"
        //let z0 = (-x)
        //printfn "%d" <| 280
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 280)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 281
        ctx.emit.comment "test281"
        //let z0 = (-4.8)
        //printfn "%d" <| 281
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 282
        ctx.emit.comment "test282"
        //let z0 = ((0.5/(-x)-(-1.5)/(-8.6))-1.3+(-7.8)-y+(-7.4)+3.5*y*(-8.4)-8.6)
        //printfn "%d" <| 282
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((0.5/(-x)-(-1.5)/(-8.6))-1.3+(-7.8)-y+(-7.4)+3.5*y*(-8.4)-8.6)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((0.5/(-p)-(-1.5)/(-8.6))-1.3+(-7.8)-q+(-7.4)+3.5*q*(-8.4)-8.6)
            z2 <== ((0.5/(-x)-(-1.5)/(-8.6))-1.3+(-7.8)-y+(-7.4)+3.5*y*(-8.4)-8.6)
            wr.tt <| (I 282)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 283
        ctx.emit.comment "test283"
        //let z0 = (-y)
        //printfn "%d" <| 283
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 283)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 284
        ctx.emit.comment "test284"
        //let z0 = x
        //printfn "%d" <| 284
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 284)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 285
        ctx.emit.comment "test285"
        //let z0 = (y+((-8.0)/(-3.0))*y/(x/((-4.2)))+(-1.4))
        //printfn "%d" <| 285
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+((-8.0)/(-3.0))*y/(x/((-4.2)))+(-1.4))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+((-8.0)/(-3.0))*q/(p/((-4.2)))+(-1.4))
            z2 <== (y+((-8.0)/(-3.0))*y/(x/((-4.2)))+(-1.4))
            wr.tt <| (I 285)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 286
        ctx.emit.comment "test286"
        //let z0 = ((-y))
        //printfn "%d" <| 286
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q))
            z2 <== ((-y))
            wr.tt <| (I 286)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 287
        ctx.emit.comment "test287"
        //let z0 = 0.5
        //printfn "%d" <| 287
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 288
        ctx.emit.comment "test288"
        //let z0 = (-y)
        //printfn "%d" <| 288
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 288)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 289
        ctx.emit.comment "test289"
        //let z0 = (-1.6)
        //printfn "%d" <| 289
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 290
        ctx.emit.comment "test290"
        //let z0 = y
        //printfn "%d" <| 290
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 290)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 291
        ctx.emit.comment "test291"
        //let z0 = ((x-(-4.6)+(y/0.4-1.3*6.0)/x/(-0.3))+y/(-x)/(-x)*(-x)-x)
        //printfn "%d" <| 291
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x-(-4.6)+(y/0.4-1.3*6.0)/x/(-0.3))+y/(-x)/(-x)*(-x)-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p-(-4.6)+(q/0.4-1.3*6.0)/p/(-0.3))+q/(-p)/(-p)*(-p)-p)
            z2 <== ((x-(-4.6)+(y/0.4-1.3*6.0)/x/(-0.3))+y/(-x)/(-x)*(-x)-x)
            wr.tt <| (I 291)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 292
        ctx.emit.comment "test292"
        //let z0 = (((-x)+((-y)/y)))
        //printfn "%d" <| 292
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)+((-y)/y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)+((-q)/q)))
            z2 <== (((-x)+((-y)/y)))
            wr.tt <| (I 292)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 293
        ctx.emit.comment "test293"
        //let z0 = y
        //printfn "%d" <| 293
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 293)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 294
        ctx.emit.comment "test294"
        //let z0 = (-3.7)
        //printfn "%d" <| 294
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 295
        ctx.emit.comment "test295"
        //let z0 = ((1.6)*((-x)-(-2.0)-5.8*y))*((-x)+(-8.7))/(1.4+(-2.4)/(-5.0)/x)*x+y+(-y)-(-8.3)*(-x)/1.8+(-y)+x-(7.4/(y)-((-y)*(-y)*(-0.2)))
        //printfn "%d" <| 295
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((1.6)*((-x)-(-2.0)-5.8*y))*((-x)+(-8.7))/(1.4+(-2.4)/(-5.0)/x)*x+y+(-y)-(-8.3)*(-x)/1.8+(-y)+x-(7.4/(y)-((-y)*(-y)*(-0.2)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((1.6)*((-p)-(-2.0)-5.8*q))*((-p)+(-8.7))/(1.4+(-2.4)/(-5.0)/p)*p+q+(-q)-(-8.3)*(-p)/1.8+(-q)+p-(7.4/(q)-((-q)*(-q)*(-0.2)))
            z2 <== ((1.6)*((-x)-(-2.0)-5.8*y))*((-x)+(-8.7))/(1.4+(-2.4)/(-5.0)/x)*x+y+(-y)-(-8.3)*(-x)/1.8+(-y)+x-(7.4/(y)-((-y)*(-y)*(-0.2)))
            wr.tt <| (I 295)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 296
        ctx.emit.comment "test296"
        //let z0 = (-2.2)
        //printfn "%d" <| 296
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 297
        ctx.emit.comment "test297"
        //let z0 = ((6.0+((-y))/(4.4*4.4)*y)/((-y)+((-x)-(-5.5)+2.7*(-1.5))-((-y)+(-x)-(-x)))-(-y)-(y*7.6-(-7.3)))
        //printfn "%d" <| 297
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((6.0+((-y))/(4.4*4.4)*y)/((-y)+((-x)-(-5.5)+2.7*(-1.5))-((-y)+(-x)-(-x)))-(-y)-(y*7.6-(-7.3)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((6.0+((-q))/(4.4*4.4)*q)/((-q)+((-p)-(-5.5)+2.7*(-1.5))-((-q)+(-p)-(-p)))-(-q)-(q*7.6-(-7.3)))
            z2 <== ((6.0+((-y))/(4.4*4.4)*y)/((-y)+((-x)-(-5.5)+2.7*(-1.5))-((-y)+(-x)-(-x)))-(-y)-(y*7.6-(-7.3)))
            wr.tt <| (I 297)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 298
        ctx.emit.comment "test298"
        //let z0 = (-1.1)
        //printfn "%d" <| 298
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 299
        ctx.emit.comment "test299"
        //let z0 = 8.6
        //printfn "%d" <| 299
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 300
        ctx.emit.comment "test300"
        //let z0 = (-7.2)
        //printfn "%d" <| 300
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 301
        ctx.emit.comment "test301"
        //let z0 = y
        //printfn "%d" <| 301
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 301)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 302
        ctx.emit.comment "test302"
        //let z0 = x
        //printfn "%d" <| 302
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 302)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 303
        ctx.emit.comment "test303"
        //let z0 = ((-0.7)+(-x))
        //printfn "%d" <| 303
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.7)+(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.7)+(-p))
            z2 <== ((-0.7)+(-x))
            wr.tt <| (I 303)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 304
        ctx.emit.comment "test304"
        //let z0 = ((5.5/y-(-8.3))*((-y)+(x/7.5/(-6.0)*y*x)+6.8))
        //printfn "%d" <| 304
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((5.5/y-(-8.3))*((-y)+(x/7.5/(-6.0)*y*x)+6.8))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((5.5/q-(-8.3))*((-q)+(p/7.5/(-6.0)*q*p)+6.8))
            z2 <== ((5.5/y-(-8.3))*((-y)+(x/7.5/(-6.0)*y*x)+6.8))
            wr.tt <| (I 304)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 305
        ctx.emit.comment "test305"
        //let z0 = ((((-y))-(y-x/(-4.7))-(x+5.8)-y+(5.6-6.3-(-x)*5.0))/(-0.3)/(x)/((8.6/y+x)/y))
        //printfn "%d" <| 305
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-y))-(y-x/(-4.7))-(x+5.8)-y+(5.6-6.3-(-x)*5.0))/(-0.3)/(x)/((8.6/y+x)/y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-q))-(q-p/(-4.7))-(p+5.8)-q+(5.6-6.3-(-p)*5.0))/(-0.3)/(p)/((8.6/q+p)/q))
            z2 <== ((((-y))-(y-x/(-4.7))-(x+5.8)-y+(5.6-6.3-(-x)*5.0))/(-0.3)/(x)/((8.6/y+x)/y))
            wr.tt <| (I 305)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 306
        ctx.emit.comment "test306"
        //let z0 = 5.6
        //printfn "%d" <| 306
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 307
        ctx.emit.comment "test307"
        //let z0 = 2.4
        //printfn "%d" <| 307
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 308
        ctx.emit.comment "test308"
        //let z0 = y
        //printfn "%d" <| 308
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 308)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 309
        ctx.emit.comment "test309"
        //let z0 = (8.4+8.8+2.2/x+(-8.7)+(-8.2))
        //printfn "%d" <| 309
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((8.4+8.8+2.2/x+(-8.7)+(-8.2))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (8.4+8.8+2.2/p+(-8.7)+(-8.2))
            z2 <== (8.4+8.8+2.2/x+(-8.7)+(-8.2))
            wr.tt <| (I 309)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 310
        ctx.emit.comment "test310"
        //let z0 = ((y-(-x)-x/((-5.0)-(-x)+(-x)/(-7.4))-(3.4-(-y)/y/3.2/(-3.1))+x)*y+(-6.7)-4.5+y-((-y)-x-((-0.0)*(-0.4)*(-x))+2.3/8.1)/(((-1.3))*(-6.5)*(x+(-2.0)/(-y)+(-y)+(-6.8))/(x)))
        //printfn "%d" <| 310
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y-(-x)-x/((-5.0)-(-x)+(-x)/(-7.4))-(3.4-(-y)/y/3.2/(-3.1))+x)*y+(-6.7)-4.5+y-((-y)-x-((-0.0)*(-0.4)*(-x))+2.3/8.1)/(((-1.3))*(-6.5)*(x+(-2.0)/(-y)+(-y)+(-6.8))/(x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q-(-p)-p/((-5.0)-(-p)+(-p)/(-7.4))-(3.4-(-q)/q/3.2/(-3.1))+p)*q+(-6.7)-4.5+q-((-q)-p-((-0.0)*(-0.4)*(-p))+2.3/8.1)/(((-1.3))*(-6.5)*(p+(-2.0)/(-q)+(-q)+(-6.8))/(p)))
            z2 <== ((y-(-x)-x/((-5.0)-(-x)+(-x)/(-7.4))-(3.4-(-y)/y/3.2/(-3.1))+x)*y+(-6.7)-4.5+y-((-y)-x-((-0.0)*(-0.4)*(-x))+2.3/8.1)/(((-1.3))*(-6.5)*(x+(-2.0)/(-y)+(-y)+(-6.8))/(x)))
            wr.tt <| (I 310)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 311
        ctx.emit.comment "test311"
        //let z0 = (-3.4)
        //printfn "%d" <| 311
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 312
        ctx.emit.comment "test312"
        //let z0 = (-1.0)
        //printfn "%d" <| 312
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 313
        ctx.emit.comment "test313"
        //let z0 = ((-4.4)*(-y)*x/(y+(-y)/x+(x*(-6.6)-(-x)*0.3*x)))
        //printfn "%d" <| 313
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-4.4)*(-y)*x/(y+(-y)/x+(x*(-6.6)-(-x)*0.3*x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-4.4)*(-q)*p/(q+(-q)/p+(p*(-6.6)-(-p)*0.3*p)))
            z2 <== ((-4.4)*(-y)*x/(y+(-y)/x+(x*(-6.6)-(-x)*0.3*x)))
            wr.tt <| (I 313)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 314
        ctx.emit.comment "test314"
        //let z0 = (((-y)-(-x)-((-x)/y+(-x)*(-x))-x+2.4))
        //printfn "%d" <| 314
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)-(-x)-((-x)/y+(-x)*(-x))-x+2.4))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)-(-p)-((-p)/q+(-p)*(-p))-p+2.4))
            z2 <== (((-y)-(-x)-((-x)/y+(-x)*(-x))-x+2.4))
            wr.tt <| (I 314)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 315
        ctx.emit.comment "test315"
        //let z0 = (-6.5)
        //printfn "%d" <| 315
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 316
        ctx.emit.comment "test316"
        //let z0 = (-y)
        //printfn "%d" <| 316
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 316)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 317
        ctx.emit.comment "test317"
        //let z0 = (-0.1)
        //printfn "%d" <| 317
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 318
        ctx.emit.comment "test318"
        //let z0 = (-4.8)
        //printfn "%d" <| 318
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 319
        ctx.emit.comment "test319"
        //let z0 = ((-x)+((1.8/(-5.3)))/((0.3/5.4+x*y/5.4)))
        //printfn "%d" <| 319
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+((1.8/(-5.3)))/((0.3/5.4+x*y/5.4)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+((1.8/(-5.3)))/((0.3/5.4+p*q/5.4)))
            z2 <== ((-x)+((1.8/(-5.3)))/((0.3/5.4+x*y/5.4)))
            wr.tt <| (I 319)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 320
        ctx.emit.comment "test320"
        //let z0 = x
        //printfn "%d" <| 320
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 320)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 321
        ctx.emit.comment "test321"
        //let z0 = ((-x)/4.7+(y)*(-4.2))
        //printfn "%d" <| 321
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)/4.7+(y)*(-4.2))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)/4.7+(q)*(-4.2))
            z2 <== ((-x)/4.7+(y)*(-4.2))
            wr.tt <| (I 321)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 322
        ctx.emit.comment "test322"
        //let z0 = ((-x)+3.8/(-5.6)/(-0.3)-y*(-y)*((-y)-(-x)*5.3*((-1.0)-x/(-4.4)/(-6.7)-(-x))*(-2.0)+(-7.6)+(-y)))
        //printfn "%d" <| 322
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+3.8/(-5.6)/(-0.3)-y*(-y)*((-y)-(-x)*5.3*((-1.0)-x/(-4.4)/(-6.7)-(-x))*(-2.0)+(-7.6)+(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+3.8/(-5.6)/(-0.3)-q*(-q)*((-q)-(-p)*5.3*((-1.0)-p/(-4.4)/(-6.7)-(-p))*(-2.0)+(-7.6)+(-q)))
            z2 <== ((-x)+3.8/(-5.6)/(-0.3)-y*(-y)*((-y)-(-x)*5.3*((-1.0)-x/(-4.4)/(-6.7)-(-x))*(-2.0)+(-7.6)+(-y)))
            wr.tt <| (I 322)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 323
        ctx.emit.comment "test323"
        //let z0 = (-y)
        //printfn "%d" <| 323
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 323)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 324
        ctx.emit.comment "test324"
        //let z0 = ((-y))
        //printfn "%d" <| 324
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q))
            z2 <== ((-y))
            wr.tt <| (I 324)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 325
        ctx.emit.comment "test325"
        //let z0 = (((-y)+y/(y+5.4+6.1-(-4.4)*3.4))/(-y)*x-(y+(-x)/((-0.4)+y*(-8.0)-1.1)+((-y)/x-(-8.2)-1.7)))
        //printfn "%d" <| 325
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)+y/(y+5.4+6.1-(-4.4)*3.4))/(-y)*x-(y+(-x)/((-0.4)+y*(-8.0)-1.1)+((-y)/x-(-8.2)-1.7)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)+q/(q+5.4+6.1-(-4.4)*3.4))/(-q)*p-(q+(-p)/((-0.4)+q*(-8.0)-1.1)+((-q)/p-(-8.2)-1.7)))
            z2 <== (((-y)+y/(y+5.4+6.1-(-4.4)*3.4))/(-y)*x-(y+(-x)/((-0.4)+y*(-8.0)-1.1)+((-y)/x-(-8.2)-1.7)))
            wr.tt <| (I 325)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 326
        ctx.emit.comment "test326"
        //let z0 = ((-x)*((-y)/(-3.7)+(-7.6)*(-1.2))*y/(-1.3)*(((-x))*((-8.8)/y*(-5.4)+6.5)-3.5/y)*((-3.8)-(x+2.5)*((-y)+(-8.0)+(-3.4)-(-8.7))))
        //printfn "%d" <| 326
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)*((-y)/(-3.7)+(-7.6)*(-1.2))*y/(-1.3)*(((-x))*((-8.8)/y*(-5.4)+6.5)-3.5/y)*((-3.8)-(x+2.5)*((-y)+(-8.0)+(-3.4)-(-8.7))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)*((-q)/(-3.7)+(-7.6)*(-1.2))*q/(-1.3)*(((-p))*((-8.8)/q*(-5.4)+6.5)-3.5/q)*((-3.8)-(p+2.5)*((-q)+(-8.0)+(-3.4)-(-8.7))))
            z2 <== ((-x)*((-y)/(-3.7)+(-7.6)*(-1.2))*y/(-1.3)*(((-x))*((-8.8)/y*(-5.4)+6.5)-3.5/y)*((-3.8)-(x+2.5)*((-y)+(-8.0)+(-3.4)-(-8.7))))
            wr.tt <| (I 326)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 327
        ctx.emit.comment "test327"
        //let z0 = (-x)
        //printfn "%d" <| 327
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 327)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 328
        ctx.emit.comment "test328"
        //let z0 = (-y)
        //printfn "%d" <| 328
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 328)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 329
        ctx.emit.comment "test329"
        //let z0 = ((-5.6)-((8.6*4.6/(-y))-x*(-6.8)+x)-(-8.8)+(((-x)+(-y)/(-y))*(x)+(-x)/y*((-y))))
        //printfn "%d" <| 329
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-5.6)-((8.6*4.6/(-y))-x*(-6.8)+x)-(-8.8)+(((-x)+(-y)/(-y))*(x)+(-x)/y*((-y))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-5.6)-((8.6*4.6/(-q))-p*(-6.8)+p)-(-8.8)+(((-p)+(-q)/(-q))*(p)+(-p)/q*((-q))))
            z2 <== ((-5.6)-((8.6*4.6/(-y))-x*(-6.8)+x)-(-8.8)+(((-x)+(-y)/(-y))*(x)+(-x)/y*((-y))))
            wr.tt <| (I 329)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 330
        ctx.emit.comment "test330"
        //let z0 = ((-5.5)*(-7.8)/3.2)-6.3/(x/8.8/y)/(-0.6)*x*(-x)+(-y)+(-1.1)*3.7/(-6.4)-1.8*((7.4)/(5.1/4.7+x)-(1.5*x))
        //printfn "%d" <| 330
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-5.5)*(-7.8)/3.2)-6.3/(x/8.8/y)/(-0.6)*x*(-x)+(-y)+(-1.1)*3.7/(-6.4)-1.8*((7.4)/(5.1/4.7+x)-(1.5*x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-5.5)*(-7.8)/3.2)-6.3/(p/8.8/q)/(-0.6)*p*(-p)+(-q)+(-1.1)*3.7/(-6.4)-1.8*((7.4)/(5.1/4.7+p)-(1.5*p))
            z2 <== ((-5.5)*(-7.8)/3.2)-6.3/(x/8.8/y)/(-0.6)*x*(-x)+(-y)+(-1.1)*3.7/(-6.4)-1.8*((7.4)/(5.1/4.7+x)-(1.5*x))
            wr.tt <| (I 330)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 331
        ctx.emit.comment "test331"
        //let z0 = ((-y)*((-3.4)/(-x)/(-2.4)-(-x)-3.4/(-1.8)))
        //printfn "%d" <| 331
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)*((-3.4)/(-x)/(-2.4)-(-x)-3.4/(-1.8)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)*((-3.4)/(-p)/(-2.4)-(-p)-3.4/(-1.8)))
            z2 <== ((-y)*((-3.4)/(-x)/(-2.4)-(-x)-3.4/(-1.8)))
            wr.tt <| (I 331)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 332
        ctx.emit.comment "test332"
        //let z0 = ((((-y)/(-0.1))+(-y)+3.6/0.6*y*(-x)/x)*((-1.8)-y-x+((-5.4)-x*(-1.4)/x))*(y/5.3/((-y)*(-y)+(-x)+0.4-(-x))+(-y))-((-4.5)/7.8/(-y)/3.0)/(-y)*((-0.4)/(-x)-y/(-8.7)*(-1.8))*(-x))
        //printfn "%d" <| 332
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-y)/(-0.1))+(-y)+3.6/0.6*y*(-x)/x)*((-1.8)-y-x+((-5.4)-x*(-1.4)/x))*(y/5.3/((-y)*(-y)+(-x)+0.4-(-x))+(-y))-((-4.5)/7.8/(-y)/3.0)/(-y)*((-0.4)/(-x)-y/(-8.7)*(-1.8))*(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-q)/(-0.1))+(-q)+3.6/0.6*q*(-p)/p)*((-1.8)-q-p+((-5.4)-p*(-1.4)/p))*(q/5.3/((-q)*(-q)+(-p)+0.4-(-p))+(-q))-((-4.5)/7.8/(-q)/3.0)/(-q)*((-0.4)/(-p)-q/(-8.7)*(-1.8))*(-p))
            z2 <== ((((-y)/(-0.1))+(-y)+3.6/0.6*y*(-x)/x)*((-1.8)-y-x+((-5.4)-x*(-1.4)/x))*(y/5.3/((-y)*(-y)+(-x)+0.4-(-x))+(-y))-((-4.5)/7.8/(-y)/3.0)/(-y)*((-0.4)/(-x)-y/(-8.7)*(-1.8))*(-x))
            wr.tt <| (I 332)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 333
        ctx.emit.comment "test333"
        //let z0 = 8.1
        //printfn "%d" <| 333
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 334
        ctx.emit.comment "test334"
        //let z0 = (7.3/(((-3.3)/x/(-6.2)/(-x))+6.4*(-y))*2.3)
        //printfn "%d" <| 334
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.3/(((-3.3)/x/(-6.2)/(-x))+6.4*(-y))*2.3)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.3/(((-3.3)/p/(-6.2)/(-p))+6.4*(-q))*2.3)
            z2 <== (7.3/(((-3.3)/x/(-6.2)/(-x))+6.4*(-y))*2.3)
            wr.tt <| (I 334)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 335
        ctx.emit.comment "test335"
        //let z0 = ((-x)-(2.5+y)*(-x)+(-y))
        //printfn "%d" <| 335
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)-(2.5+y)*(-x)+(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)-(2.5+q)*(-p)+(-q))
            z2 <== ((-x)-(2.5+y)*(-x)+(-y))
            wr.tt <| (I 335)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 336
        ctx.emit.comment "test336"
        //let z0 = (x-(-5.7)*(-5.2)/(-1.3)+(-2.1)*y/(((-4.1)-x))+x+((-7.3)-y+((-2.7))+x*((-y))))
        //printfn "%d" <| 336
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x-(-5.7)*(-5.2)/(-1.3)+(-2.1)*y/(((-4.1)-x))+x+((-7.3)-y+((-2.7))+x*((-y))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p-(-5.7)*(-5.2)/(-1.3)+(-2.1)*q/(((-4.1)-p))+p+((-7.3)-q+((-2.7))+p*((-q))))
            z2 <== (x-(-5.7)*(-5.2)/(-1.3)+(-2.1)*y/(((-4.1)-x))+x+((-7.3)-y+((-2.7))+x*((-y))))
            wr.tt <| (I 336)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 337
        ctx.emit.comment "test337"
        //let z0 = ((-4.3)-(-x)+(x-(-4.3)-4.8)+(0.8*7.5-x+3.7)*x-(-0.4)*(((-1.8)-1.7-(-3.4)-(-1.5))*(-y)*((-y)*y/(-y)-6.8/(-6.4))))
        //printfn "%d" <| 337
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-4.3)-(-x)+(x-(-4.3)-4.8)+(0.8*7.5-x+3.7)*x-(-0.4)*(((-1.8)-1.7-(-3.4)-(-1.5))*(-y)*((-y)*y/(-y)-6.8/(-6.4))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-4.3)-(-p)+(p-(-4.3)-4.8)+(0.8*7.5-p+3.7)*p-(-0.4)*(((-1.8)-1.7-(-3.4)-(-1.5))*(-q)*((-q)*q/(-q)-6.8/(-6.4))))
            z2 <== ((-4.3)-(-x)+(x-(-4.3)-4.8)+(0.8*7.5-x+3.7)*x-(-0.4)*(((-1.8)-1.7-(-3.4)-(-1.5))*(-y)*((-y)*y/(-y)-6.8/(-6.4))))
            wr.tt <| (I 337)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 338
        ctx.emit.comment "test338"
        //let z0 = 2.7
        //printfn "%d" <| 338
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 339
        ctx.emit.comment "test339"
        //let z0 = x
        //printfn "%d" <| 339
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 339)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 340
        ctx.emit.comment "test340"
        //let z0 = ((-x)+y*y-((x-(-y)*8.4)/(-2.5)-(6.6/(-x)*x)*((-2.4)/(-7.5)+(-1.1)-(-1.2)*(-7.0))+(5.8-3.1*y-(-5.6)-0.0))/(-6.0))
        //printfn "%d" <| 340
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+y*y-((x-(-y)*8.4)/(-2.5)-(6.6/(-x)*x)*((-2.4)/(-7.5)+(-1.1)-(-1.2)*(-7.0))+(5.8-3.1*y-(-5.6)-0.0))/(-6.0))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+q*q-((p-(-q)*8.4)/(-2.5)-(6.6/(-p)*p)*((-2.4)/(-7.5)+(-1.1)-(-1.2)*(-7.0))+(5.8-3.1*q-(-5.6)-0.0))/(-6.0))
            z2 <== ((-x)+y*y-((x-(-y)*8.4)/(-2.5)-(6.6/(-x)*x)*((-2.4)/(-7.5)+(-1.1)-(-1.2)*(-7.0))+(5.8-3.1*y-(-5.6)-0.0))/(-6.0))
            wr.tt <| (I 340)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 341
        ctx.emit.comment "test341"
        //let z0 = 7.5
        //printfn "%d" <| 341
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 342
        ctx.emit.comment "test342"
        //let z0 = ((((-y)*3.1-(-x)+(-x))*(-x)+y/(-4.4)-(-1.5)-7.6/(-0.1)-4.3*((-0.2)-y/3.3))*4.4/((-y)*(-5.1)*((-x))-(y*7.1+y*6.4*y)*6.7)/(-0.3))
        //printfn "%d" <| 342
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-y)*3.1-(-x)+(-x))*(-x)+y/(-4.4)-(-1.5)-7.6/(-0.1)-4.3*((-0.2)-y/3.3))*4.4/((-y)*(-5.1)*((-x))-(y*7.1+y*6.4*y)*6.7)/(-0.3))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-q)*3.1-(-p)+(-p))*(-p)+q/(-4.4)-(-1.5)-7.6/(-0.1)-4.3*((-0.2)-q/3.3))*4.4/((-q)*(-5.1)*((-p))-(q*7.1+q*6.4*q)*6.7)/(-0.3))
            z2 <== ((((-y)*3.1-(-x)+(-x))*(-x)+y/(-4.4)-(-1.5)-7.6/(-0.1)-4.3*((-0.2)-y/3.3))*4.4/((-y)*(-5.1)*((-x))-(y*7.1+y*6.4*y)*6.7)/(-0.3))
            wr.tt <| (I 342)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 343
        ctx.emit.comment "test343"
        //let z0 = (-y)
        //printfn "%d" <| 343
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 343)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 344
        ctx.emit.comment "test344"
        //let z0 = 1.5
        //printfn "%d" <| 344
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 345
        ctx.emit.comment "test345"
        //let z0 = (8.7-(-x)+((0.3)-(3.5*(-y)/y/7.2+(-2.7))*(-6.7)-(-3.4))/((-6.6)+(-1.3)))
        //printfn "%d" <| 345
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((8.7-(-x)+((0.3)-(3.5*(-y)/y/7.2+(-2.7))*(-6.7)-(-3.4))/((-6.6)+(-1.3)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (8.7-(-p)+((0.3)-(3.5*(-q)/q/7.2+(-2.7))*(-6.7)-(-3.4))/((-6.6)+(-1.3)))
            z2 <== (8.7-(-x)+((0.3)-(3.5*(-y)/y/7.2+(-2.7))*(-6.7)-(-3.4))/((-6.6)+(-1.3)))
            wr.tt <| (I 345)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 346
        ctx.emit.comment "test346"
        //let z0 = ((-0.1))
        //printfn "%d" <| 346
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 347
        ctx.emit.comment "test347"
        //let z0 = ((-y)+1.0+(y/((-0.5)+(-6.3)-(-x)/(-y))/(-x))-(2.8))
        //printfn "%d" <| 347
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)+1.0+(y/((-0.5)+(-6.3)-(-x)/(-y))/(-x))-(2.8))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)+1.0+(q/((-0.5)+(-6.3)-(-p)/(-q))/(-p))-(2.8))
            z2 <== ((-y)+1.0+(y/((-0.5)+(-6.3)-(-x)/(-y))/(-x))-(2.8))
            wr.tt <| (I 347)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 348
        ctx.emit.comment "test348"
        //let z0 = x
        //printfn "%d" <| 348
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 348)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 349
        ctx.emit.comment "test349"
        //let z0 = 2.2
        //printfn "%d" <| 349
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 350
        ctx.emit.comment "test350"
        //let z0 = (-y)
        //printfn "%d" <| 350
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 350)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 351
        ctx.emit.comment "test351"
        //let z0 = x
        //printfn "%d" <| 351
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 351)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 352
        ctx.emit.comment "test352"
        //let z0 = (3.7/x*(-y)*(-6.6)-3.2/(-4.2)+y/y+(-7.1))/(((-3.8)+5.4))-5.1/(-7.2)-4.5
        //printfn "%d" <| 352
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((3.7/x*(-y)*(-6.6)-3.2/(-4.2)+y/y+(-7.1))/(((-3.8)+5.4))-5.1/(-7.2)-4.5).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (3.7/p*(-q)*(-6.6)-3.2/(-4.2)+q/q+(-7.1))/(((-3.8)+5.4))-5.1/(-7.2)-4.5
            z2 <== (3.7/x*(-y)*(-6.6)-3.2/(-4.2)+y/y+(-7.1))/(((-3.8)+5.4))-5.1/(-7.2)-4.5
            wr.tt <| (I 352)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 353
        ctx.emit.comment "test353"
        //let z0 = ((((-8.8)+3.3+(-x)/(-6.7)*8.6)*(x-y+(-x)+1.0/4.3)-((-5.4)*x/(-x)*x)+8.5)-(-2.3)/(-7.7)/(6.6-(-1.1))/3.6*6.2)
        //printfn "%d" <| 353
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-8.8)+3.3+(-x)/(-6.7)*8.6)*(x-y+(-x)+1.0/4.3)-((-5.4)*x/(-x)*x)+8.5)-(-2.3)/(-7.7)/(6.6-(-1.1))/3.6*6.2)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-8.8)+3.3+(-p)/(-6.7)*8.6)*(p-q+(-p)+1.0/4.3)-((-5.4)*p/(-p)*p)+8.5)-(-2.3)/(-7.7)/(6.6-(-1.1))/3.6*6.2)
            z2 <== ((((-8.8)+3.3+(-x)/(-6.7)*8.6)*(x-y+(-x)+1.0/4.3)-((-5.4)*x/(-x)*x)+8.5)-(-2.3)/(-7.7)/(6.6-(-1.1))/3.6*6.2)
            wr.tt <| (I 353)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 354
        ctx.emit.comment "test354"
        //let z0 = (((x/(-y)/(-2.8)*(-y)))/(-5.5)*((-x)*(-x)-6.8-(-y)/y)+((-y)/(y+7.3+4.2*(-x)+(-y)))-(-4.6))
        //printfn "%d" <| 354
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((x/(-y)/(-2.8)*(-y)))/(-5.5)*((-x)*(-x)-6.8-(-y)/y)+((-y)/(y+7.3+4.2*(-x)+(-y)))-(-4.6))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((p/(-q)/(-2.8)*(-q)))/(-5.5)*((-p)*(-p)-6.8-(-q)/q)+((-q)/(q+7.3+4.2*(-p)+(-q)))-(-4.6))
            z2 <== (((x/(-y)/(-2.8)*(-y)))/(-5.5)*((-x)*(-x)-6.8-(-y)/y)+((-y)/(y+7.3+4.2*(-x)+(-y)))-(-4.6))
            wr.tt <| (I 354)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 355
        ctx.emit.comment "test355"
        //let z0 = 6.7
        //printfn "%d" <| 355
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 356
        ctx.emit.comment "test356"
        //let z0 = (-2.8)
        //printfn "%d" <| 356
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 357
        ctx.emit.comment "test357"
        //let z0 = y
        //printfn "%d" <| 357
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 357)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 358
        ctx.emit.comment "test358"
        //let z0 = x
        //printfn "%d" <| 358
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 358)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 359
        ctx.emit.comment "test359"
        //let z0 = ((-y)-((-x)/7.5)*(-y)/(-x))
        //printfn "%d" <| 359
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)-((-x)/7.5)*(-y)/(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)-((-p)/7.5)*(-q)/(-p))
            z2 <== ((-y)-((-x)/7.5)*(-y)/(-x))
            wr.tt <| (I 359)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 360
        ctx.emit.comment "test360"
        //let z0 = ((-y)+3.7)
        //printfn "%d" <| 360
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)+3.7)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)+3.7)
            z2 <== ((-y)+3.7)
            wr.tt <| (I 360)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 361
        ctx.emit.comment "test361"
        //let z0 = 3.4
        //printfn "%d" <| 361
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 362
        ctx.emit.comment "test362"
        //let z0 = ((-5.8)-((-y)+x/x))
        //printfn "%d" <| 362
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-5.8)-((-y)+x/x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-5.8)-((-q)+p/p))
            z2 <== ((-5.8)-((-y)+x/x))
            wr.tt <| (I 362)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 363
        ctx.emit.comment "test363"
        //let z0 = ((-4.3)-1.8-(((-x)/(-4.7))+((-4.1)+x*y)*(-y)+4.1/4.8))
        //printfn "%d" <| 363
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-4.3)-1.8-(((-x)/(-4.7))+((-4.1)+x*y)*(-y)+4.1/4.8))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-4.3)-1.8-(((-p)/(-4.7))+((-4.1)+p*q)*(-q)+4.1/4.8))
            z2 <== ((-4.3)-1.8-(((-x)/(-4.7))+((-4.1)+x*y)*(-y)+4.1/4.8))
            wr.tt <| (I 363)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 364
        ctx.emit.comment "test364"
        //let z0 = ((-x)+(((-0.8)+(-0.3)/1.7-(-8.2)/3.1)/y))
        //printfn "%d" <| 364
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+(((-0.8)+(-0.3)/1.7-(-8.2)/3.1)/y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+(((-0.8)+(-0.3)/1.7-(-8.2)/3.1)/q))
            z2 <== ((-x)+(((-0.8)+(-0.3)/1.7-(-8.2)/3.1)/y))
            wr.tt <| (I 364)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 365
        ctx.emit.comment "test365"
        //let z0 = (((-y)/(8.8+(-7.3))/8.2*(-4.1)/(-y))/(-0.6)-y+x)
        //printfn "%d" <| 365
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)/(8.8+(-7.3))/8.2*(-4.1)/(-y))/(-0.6)-y+x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)/(8.8+(-7.3))/8.2*(-4.1)/(-q))/(-0.6)-q+p)
            z2 <== (((-y)/(8.8+(-7.3))/8.2*(-4.1)/(-y))/(-0.6)-y+x)
            wr.tt <| (I 365)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 366
        ctx.emit.comment "test366"
        //let z0 = 3.5*x-(2.1/(-5.3)-(-7.4)+(-y)/x)*(6.4/(-0.6)+y/(-5.0)+(-5.1))*((-x)*4.3+x-(-3.0)-4.1*7.1)+x-8.7
        //printfn "%d" <| 366
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (3.5*x-(2.1/(-5.3)-(-7.4)+(-y)/x)*(6.4/(-0.6)+y/(-5.0)+(-5.1))*((-x)*4.3+x-(-3.0)-4.1*7.1)+x-8.7).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 3.5*p-(2.1/(-5.3)-(-7.4)+(-q)/p)*(6.4/(-0.6)+q/(-5.0)+(-5.1))*((-p)*4.3+p-(-3.0)-4.1*7.1)+p-8.7
            z2 <== 3.5*x-(2.1/(-5.3)-(-7.4)+(-y)/x)*(6.4/(-0.6)+y/(-5.0)+(-5.1))*((-x)*4.3+x-(-3.0)-4.1*7.1)+x-8.7
            wr.tt <| (I 366)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 367
        ctx.emit.comment "test367"
        //let z0 = (-4.2)
        //printfn "%d" <| 367
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 368
        ctx.emit.comment "test368"
        //let z0 = x
        //printfn "%d" <| 368
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 368)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 369
        ctx.emit.comment "test369"
        //let z0 = 6.5
        //printfn "%d" <| 369
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 370
        ctx.emit.comment "test370"
        //let z0 = (((-4.6)-((-y))-((-6.1)+x-x/3.0))-((6.1/4.2-(-x)/(-4.7)+(-x)))/x/(-x)/((-y)/(7.6*(-x)-x/(-x))*(8.5*5.3+(-8.5)-(-x))+((-1.6)+(-x))))
        //printfn "%d" <| 370
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-4.6)-((-y))-((-6.1)+x-x/3.0))-((6.1/4.2-(-x)/(-4.7)+(-x)))/x/(-x)/((-y)/(7.6*(-x)-x/(-x))*(8.5*5.3+(-8.5)-(-x))+((-1.6)+(-x))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-4.6)-((-q))-((-6.1)+p-p/3.0))-((6.1/4.2-(-p)/(-4.7)+(-p)))/p/(-p)/((-q)/(7.6*(-p)-p/(-p))*(8.5*5.3+(-8.5)-(-p))+((-1.6)+(-p))))
            z2 <== (((-4.6)-((-y))-((-6.1)+x-x/3.0))-((6.1/4.2-(-x)/(-4.7)+(-x)))/x/(-x)/((-y)/(7.6*(-x)-x/(-x))*(8.5*5.3+(-8.5)-(-x))+((-1.6)+(-x))))
            wr.tt <| (I 370)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 371
        ctx.emit.comment "test371"
        //let z0 = ((-x)-0.1)-(-5.7)/(-y)-4.1*3.7/(1.6+(-y)*(-y)-(-6.5))*((-y)/y/6.7*(-2.4)+y)-((-y)+(-5.6))/((-1.8))/0.5*(-x)+2.3*(-x)*((3.4*y/3.0))
        //printfn "%d" <| 371
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)-0.1)-(-5.7)/(-y)-4.1*3.7/(1.6+(-y)*(-y)-(-6.5))*((-y)/y/6.7*(-2.4)+y)-((-y)+(-5.6))/((-1.8))/0.5*(-x)+2.3*(-x)*((3.4*y/3.0))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)-0.1)-(-5.7)/(-q)-4.1*3.7/(1.6+(-q)*(-q)-(-6.5))*((-q)/q/6.7*(-2.4)+q)-((-q)+(-5.6))/((-1.8))/0.5*(-p)+2.3*(-p)*((3.4*q/3.0))
            z2 <== ((-x)-0.1)-(-5.7)/(-y)-4.1*3.7/(1.6+(-y)*(-y)-(-6.5))*((-y)/y/6.7*(-2.4)+y)-((-y)+(-5.6))/((-1.8))/0.5*(-x)+2.3*(-x)*((3.4*y/3.0))
            wr.tt <| (I 371)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 372
        ctx.emit.comment "test372"
        //let z0 = ((((-x)+(-x)-(-4.2)-5.7)-((-x)+y/(-2.6)))*(x)*(y-(-2.1)+((-y)))*y*((y/(-8.8)+x+5.4)/x+(-2.7)/((-1.8)-(-y)*5.7+1.2)+7.2))
        //printfn "%d" <| 372
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-x)+(-x)-(-4.2)-5.7)-((-x)+y/(-2.6)))*(x)*(y-(-2.1)+((-y)))*y*((y/(-8.8)+x+5.4)/x+(-2.7)/((-1.8)-(-y)*5.7+1.2)+7.2))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-p)+(-p)-(-4.2)-5.7)-((-p)+q/(-2.6)))*(p)*(q-(-2.1)+((-q)))*q*((q/(-8.8)+p+5.4)/p+(-2.7)/((-1.8)-(-q)*5.7+1.2)+7.2))
            z2 <== ((((-x)+(-x)-(-4.2)-5.7)-((-x)+y/(-2.6)))*(x)*(y-(-2.1)+((-y)))*y*((y/(-8.8)+x+5.4)/x+(-2.7)/((-1.8)-(-y)*5.7+1.2)+7.2))
            wr.tt <| (I 372)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 373
        ctx.emit.comment "test373"
        //let z0 = 4.1+(-0.6)-((-6.4)*y)/(y+y/8.5)-((-y)-6.6*(-x)-y)/y*(x+y*(-1.2)-6.1+0.7*(-y))*1.8
        //printfn "%d" <| 373
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (4.1+(-0.6)-((-6.4)*y)/(y+y/8.5)-((-y)-6.6*(-x)-y)/y*(x+y*(-1.2)-6.1+0.7*(-y))*1.8).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 4.1+(-0.6)-((-6.4)*q)/(q+q/8.5)-((-q)-6.6*(-p)-q)/q*(p+q*(-1.2)-6.1+0.7*(-q))*1.8
            z2 <== 4.1+(-0.6)-((-6.4)*y)/(y+y/8.5)-((-y)-6.6*(-x)-y)/y*(x+y*(-1.2)-6.1+0.7*(-y))*1.8
            wr.tt <| (I 373)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 374
        ctx.emit.comment "test374"
        //let z0 = (((5.2)+x/x+7.3)-(6.2-(-x)-x)-x+(-y)-((-x)-4.5))
        //printfn "%d" <| 374
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((5.2)+x/x+7.3)-(6.2-(-x)-x)-x+(-y)-((-x)-4.5))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((5.2)+p/p+7.3)-(6.2-(-p)-p)-p+(-q)-((-p)-4.5))
            z2 <== (((5.2)+x/x+7.3)-(6.2-(-x)-x)-x+(-y)-((-x)-4.5))
            wr.tt <| (I 374)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 375
        ctx.emit.comment "test375"
        //let z0 = (((-1.7))/5.0*((-y)+(-y)+1.1-(-y)*x-(-x)/x)*x)
        //printfn "%d" <| 375
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-1.7))/5.0*((-y)+(-y)+1.1-(-y)*x-(-x)/x)*x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-1.7))/5.0*((-q)+(-q)+1.1-(-q)*p-(-p)/p)*p)
            z2 <== (((-1.7))/5.0*((-y)+(-y)+1.1-(-y)*x-(-x)/x)*x)
            wr.tt <| (I 375)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 376
        ctx.emit.comment "test376"
        //let z0 = (((-7.0)+y+(-y)+(-x))*(-x))*(-0.5)*(((-y)*4.2-(-y))+((-y)/(-2.2)+(-x)))+((-3.7))+(-x)
        //printfn "%d" <| 376
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-7.0)+y+(-y)+(-x))*(-x))*(-0.5)*(((-y)*4.2-(-y))+((-y)/(-2.2)+(-x)))+((-3.7))+(-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-7.0)+q+(-q)+(-p))*(-p))*(-0.5)*(((-q)*4.2-(-q))+((-q)/(-2.2)+(-p)))+((-3.7))+(-p)
            z2 <== (((-7.0)+y+(-y)+(-x))*(-x))*(-0.5)*(((-y)*4.2-(-y))+((-y)/(-2.2)+(-x)))+((-3.7))+(-x)
            wr.tt <| (I 376)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 377
        ctx.emit.comment "test377"
        //let z0 = (((7.0-(-y)*y/6.1)/((-x)*(-8.1))+((-2.7)/(-2.5))-6.8/x-(-5.3)*(-y))+(1.3+(2.5+(-4.2)/4.2-x)*((-x)/y/(-y)+y/2.7)*(y))-(((-y)-(-y)-y/(-6.4)*4.3)/(-y)-(x+(-5.3)+(-3.0))-((-6.2)/4.5))*(1.4/x))
        //printfn "%d" <| 377
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((7.0-(-y)*y/6.1)/((-x)*(-8.1))+((-2.7)/(-2.5))-6.8/x-(-5.3)*(-y))+(1.3+(2.5+(-4.2)/4.2-x)*((-x)/y/(-y)+y/2.7)*(y))-(((-y)-(-y)-y/(-6.4)*4.3)/(-y)-(x+(-5.3)+(-3.0))-((-6.2)/4.5))*(1.4/x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((7.0-(-q)*q/6.1)/((-p)*(-8.1))+((-2.7)/(-2.5))-6.8/p-(-5.3)*(-q))+(1.3+(2.5+(-4.2)/4.2-p)*((-p)/q/(-q)+q/2.7)*(q))-(((-q)-(-q)-q/(-6.4)*4.3)/(-q)-(p+(-5.3)+(-3.0))-((-6.2)/4.5))*(1.4/p))
            z2 <== (((7.0-(-y)*y/6.1)/((-x)*(-8.1))+((-2.7)/(-2.5))-6.8/x-(-5.3)*(-y))+(1.3+(2.5+(-4.2)/4.2-x)*((-x)/y/(-y)+y/2.7)*(y))-(((-y)-(-y)-y/(-6.4)*4.3)/(-y)-(x+(-5.3)+(-3.0))-((-6.2)/4.5))*(1.4/x))
            wr.tt <| (I 377)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 378
        ctx.emit.comment "test378"
        //let z0 = (-2.0)
        //printfn "%d" <| 378
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 379
        ctx.emit.comment "test379"
        //let z0 = 0.6
        //printfn "%d" <| 379
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 380
        ctx.emit.comment "test380"
        //let z0 = ((-y)/3.7/((-5.7)+1.1+x)*0.4/(5.8*(-x)/(-x)+1.5)-(x+2.3)/(-6.5)*y-((3.8+(-7.4)-5.3+y)-((-y)/5.2-5.5+2.2)-x/(7.4+(-4.2)-(-0.7)*6.6)))
        //printfn "%d" <| 380
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)/3.7/((-5.7)+1.1+x)*0.4/(5.8*(-x)/(-x)+1.5)-(x+2.3)/(-6.5)*y-((3.8+(-7.4)-5.3+y)-((-y)/5.2-5.5+2.2)-x/(7.4+(-4.2)-(-0.7)*6.6)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)/3.7/((-5.7)+1.1+p)*0.4/(5.8*(-p)/(-p)+1.5)-(p+2.3)/(-6.5)*q-((3.8+(-7.4)-5.3+q)-((-q)/5.2-5.5+2.2)-p/(7.4+(-4.2)-(-0.7)*6.6)))
            z2 <== ((-y)/3.7/((-5.7)+1.1+x)*0.4/(5.8*(-x)/(-x)+1.5)-(x+2.3)/(-6.5)*y-((3.8+(-7.4)-5.3+y)-((-y)/5.2-5.5+2.2)-x/(7.4+(-4.2)-(-0.7)*6.6)))
            wr.tt <| (I 380)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 381
        ctx.emit.comment "test381"
        //let z0 = 1.2
        //printfn "%d" <| 381
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 382
        ctx.emit.comment "test382"
        //let z0 = 8.3
        //printfn "%d" <| 382
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 383
        ctx.emit.comment "test383"
        //let z0 = (-5.8)
        //printfn "%d" <| 383
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 384
        ctx.emit.comment "test384"
        //let z0 = 5.1
        //printfn "%d" <| 384
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 385
        ctx.emit.comment "test385"
        //let z0 = y
        //printfn "%d" <| 385
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 385)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 386
        ctx.emit.comment "test386"
        //let z0 = 8.5
        //printfn "%d" <| 386
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 387
        ctx.emit.comment "test387"
        //let z0 = (y-(2.0)*y)
        //printfn "%d" <| 387
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y-(2.0)*y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q-(2.0)*q)
            z2 <== (y-(2.0)*y)
            wr.tt <| (I 387)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 388
        ctx.emit.comment "test388"
        //let z0 = (7.0*((-7.1)/4.5*((-y)))+0.5)
        //printfn "%d" <| 388
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.0*((-7.1)/4.5*((-y)))+0.5)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.0*((-7.1)/4.5*((-q)))+0.5)
            z2 <== (7.0*((-7.1)/4.5*((-y)))+0.5)
            wr.tt <| (I 388)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 389
        ctx.emit.comment "test389"
        //let z0 = ((((-1.7)-8.8)/(-6.0))/8.8-(((-x)-x)/(x*(-8.7)+y+4.2)/((-y)*(-y)*(-5.1))))
        //printfn "%d" <| 389
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-1.7)-8.8)/(-6.0))/8.8-(((-x)-x)/(x*(-8.7)+y+4.2)/((-y)*(-y)*(-5.1))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-1.7)-8.8)/(-6.0))/8.8-(((-p)-p)/(p*(-8.7)+q+4.2)/((-q)*(-q)*(-5.1))))
            z2 <== ((((-1.7)-8.8)/(-6.0))/8.8-(((-x)-x)/(x*(-8.7)+y+4.2)/((-y)*(-y)*(-5.1))))
            wr.tt <| (I 389)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 390
        ctx.emit.comment "test390"
        //let z0 = (-0.0)
        //printfn "%d" <| 390
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 391
        ctx.emit.comment "test391"
        //let z0 = (y/(((-y)+(-y))-((-x))-2.1-x/1.1/2.1)+(((-x)*(-3.4))*(-y))-x-((-6.8)+5.1-(-4.8)-(-x)*4.5)-(8.6+5.6*(-x))+(-y)-2.0)
        //printfn "%d" <| 391
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y/(((-y)+(-y))-((-x))-2.1-x/1.1/2.1)+(((-x)*(-3.4))*(-y))-x-((-6.8)+5.1-(-4.8)-(-x)*4.5)-(8.6+5.6*(-x))+(-y)-2.0)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q/(((-q)+(-q))-((-p))-2.1-p/1.1/2.1)+(((-p)*(-3.4))*(-q))-p-((-6.8)+5.1-(-4.8)-(-p)*4.5)-(8.6+5.6*(-p))+(-q)-2.0)
            z2 <== (y/(((-y)+(-y))-((-x))-2.1-x/1.1/2.1)+(((-x)*(-3.4))*(-y))-x-((-6.8)+5.1-(-4.8)-(-x)*4.5)-(8.6+5.6*(-x))+(-y)-2.0)
            wr.tt <| (I 391)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 392
        ctx.emit.comment "test392"
        //let z0 = 5.7
        //printfn "%d" <| 392
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 393
        ctx.emit.comment "test393"
        //let z0 = 1.5
        //printfn "%d" <| 393
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 394
        ctx.emit.comment "test394"
        //let z0 = 3.6
        //printfn "%d" <| 394
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 395
        ctx.emit.comment "test395"
        //let z0 = x
        //printfn "%d" <| 395
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 395)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 396
        ctx.emit.comment "test396"
        //let z0 = (-x)
        //printfn "%d" <| 396
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 396)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 397
        ctx.emit.comment "test397"
        //let z0 = (-y)
        //printfn "%d" <| 397
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 397)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 398
        ctx.emit.comment "test398"
        //let z0 = y*((x+y*3.0-(-x))*(-1.2)*((-2.1)*y-(-x)-0.2+(-x))*(-y)+(-x))-1.4
        //printfn "%d" <| 398
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y*((x+y*3.0-(-x))*(-1.2)*((-2.1)*y-(-x)-0.2+(-x))*(-y)+(-x))-1.4).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q*((p+q*3.0-(-p))*(-1.2)*((-2.1)*q-(-p)-0.2+(-p))*(-q)+(-p))-1.4
            z2 <== y*((x+y*3.0-(-x))*(-1.2)*((-2.1)*y-(-x)-0.2+(-x))*(-y)+(-x))-1.4
            wr.tt <| (I 398)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 399
        ctx.emit.comment "test399"
        //let z0 = (-y)
        //printfn "%d" <| 399
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 399)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 400
        ctx.emit.comment "test400"
        //let z0 = 4.7
        //printfn "%d" <| 400
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 401
        ctx.emit.comment "test401"
        //let z0 = 0.1
        //printfn "%d" <| 401
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 402
        ctx.emit.comment "test402"
        //let z0 = (x-((3.1)/1.7)*(5.0/8.2-y*(7.3/6.1-0.3/(-7.0)))-((7.5-(-x))))
        //printfn "%d" <| 402
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x-((3.1)/1.7)*(5.0/8.2-y*(7.3/6.1-0.3/(-7.0)))-((7.5-(-x))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p-((3.1)/1.7)*(5.0/8.2-q*(7.3/6.1-0.3/(-7.0)))-((7.5-(-p))))
            z2 <== (x-((3.1)/1.7)*(5.0/8.2-y*(7.3/6.1-0.3/(-7.0)))-((7.5-(-x))))
            wr.tt <| (I 402)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 403
        ctx.emit.comment "test403"
        //let z0 = (-7.0)
        //printfn "%d" <| 403
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 404
        ctx.emit.comment "test404"
        //let z0 = (-y)
        //printfn "%d" <| 404
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 404)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 405
        ctx.emit.comment "test405"
        //let z0 = (-y)
        //printfn "%d" <| 405
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 405)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 406
        ctx.emit.comment "test406"
        //let z0 = y
        //printfn "%d" <| 406
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 406)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 407
        ctx.emit.comment "test407"
        //let z0 = (0.5/y+((-8.6)+x/y-(-x)+x-x/(-4.0)-(-y))/((-8.0)*3.4)*(-0.2)+((-y)*(-y)*1.2/(-4.6))-(x*y)/(-y))
        //printfn "%d" <| 407
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((0.5/y+((-8.6)+x/y-(-x)+x-x/(-4.0)-(-y))/((-8.0)*3.4)*(-0.2)+((-y)*(-y)*1.2/(-4.6))-(x*y)/(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (0.5/q+((-8.6)+p/q-(-p)+p-p/(-4.0)-(-q))/((-8.0)*3.4)*(-0.2)+((-q)*(-q)*1.2/(-4.6))-(p*q)/(-q))
            z2 <== (0.5/y+((-8.6)+x/y-(-x)+x-x/(-4.0)-(-y))/((-8.0)*3.4)*(-0.2)+((-y)*(-y)*1.2/(-4.6))-(x*y)/(-y))
            wr.tt <| (I 407)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 408
        ctx.emit.comment "test408"
        //let z0 = (-4.3)
        //printfn "%d" <| 408
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 409
        ctx.emit.comment "test409"
        //let z0 = 1.1
        //printfn "%d" <| 409
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 410
        ctx.emit.comment "test410"
        //let z0 = (-x)+(-3.7)
        //printfn "%d" <| 410
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)+(-3.7)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)+(-3.7)
            z2 <== (-x)+(-3.7)
            wr.tt <| (I 410)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 411
        ctx.emit.comment "test411"
        //let z0 = (x+6.8)
        //printfn "%d" <| 411
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x+6.8)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p+6.8)
            z2 <== (x+6.8)
            wr.tt <| (I 411)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 412
        ctx.emit.comment "test412"
        //let z0 = (-x)
        //printfn "%d" <| 412
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 412)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 413
        ctx.emit.comment "test413"
        //let z0 = (-5.6)
        //printfn "%d" <| 413
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 414
        ctx.emit.comment "test414"
        //let z0 = (-x)
        //printfn "%d" <| 414
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 414)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 415
        ctx.emit.comment "test415"
        //let z0 = ((y/((-y))))
        //printfn "%d" <| 415
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y/((-y))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q/((-q))))
            z2 <== ((y/((-y))))
            wr.tt <| (I 415)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 416
        ctx.emit.comment "test416"
        //let z0 = ((-6.1)/x*(6.3*(-x)-5.7/4.1)/(x*x/2.0)-((-y)/(-7.8)+(-2.5))+(y-((-7.4)-(-y)/(-x)/(-x)))*((-x)-(y/1.2*(-y)+(-y))/x-((-4.4)*x)/(-8.3)))
        //printfn "%d" <| 416
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-6.1)/x*(6.3*(-x)-5.7/4.1)/(x*x/2.0)-((-y)/(-7.8)+(-2.5))+(y-((-7.4)-(-y)/(-x)/(-x)))*((-x)-(y/1.2*(-y)+(-y))/x-((-4.4)*x)/(-8.3)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-6.1)/p*(6.3*(-p)-5.7/4.1)/(p*p/2.0)-((-q)/(-7.8)+(-2.5))+(q-((-7.4)-(-q)/(-p)/(-p)))*((-p)-(q/1.2*(-q)+(-q))/p-((-4.4)*p)/(-8.3)))
            z2 <== ((-6.1)/x*(6.3*(-x)-5.7/4.1)/(x*x/2.0)-((-y)/(-7.8)+(-2.5))+(y-((-7.4)-(-y)/(-x)/(-x)))*((-x)-(y/1.2*(-y)+(-y))/x-((-4.4)*x)/(-8.3)))
            wr.tt <| (I 416)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 417
        ctx.emit.comment "test417"
        //let z0 = (-1.7)
        //printfn "%d" <| 417
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 418
        ctx.emit.comment "test418"
        //let z0 = (-y)
        //printfn "%d" <| 418
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 418)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 419
        ctx.emit.comment "test419"
        //let z0 = ((-0.1)*4.2)
        //printfn "%d" <| 419
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 420
        ctx.emit.comment "test420"
        //let z0 = (((y-x*y)/2.3*y/(-y))*((y+7.6/(-8.3)*2.1+x)+(-7.8)*((-y)*(-y)+2.7)*y-5.8)*(-x)-3.1)
        //printfn "%d" <| 420
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((y-x*y)/2.3*y/(-y))*((y+7.6/(-8.3)*2.1+x)+(-7.8)*((-y)*(-y)+2.7)*y-5.8)*(-x)-3.1)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((q-p*q)/2.3*q/(-q))*((q+7.6/(-8.3)*2.1+p)+(-7.8)*((-q)*(-q)+2.7)*q-5.8)*(-p)-3.1)
            z2 <== (((y-x*y)/2.3*y/(-y))*((y+7.6/(-8.3)*2.1+x)+(-7.8)*((-y)*(-y)+2.7)*y-5.8)*(-x)-3.1)
            wr.tt <| (I 420)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 421
        ctx.emit.comment "test421"
        //let z0 = x
        //printfn "%d" <| 421
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 421)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 422
        ctx.emit.comment "test422"
        //let z0 = (((-x)+(-5.5))*(-x)+(-7.7))
        //printfn "%d" <| 422
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)+(-5.5))*(-x)+(-7.7))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)+(-5.5))*(-p)+(-7.7))
            z2 <== (((-x)+(-5.5))*(-x)+(-7.7))
            wr.tt <| (I 422)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 423
        ctx.emit.comment "test423"
        //let z0 = (-8.2)
        //printfn "%d" <| 423
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 424
        ctx.emit.comment "test424"
        //let z0 = (y+((y-(-0.8))-y-x)*((-x)*4.5*y/(-x))/((6.8+3.4+(-0.1))+(-y))-((-8.6)/(-3.0)/(-x)-x))
        //printfn "%d" <| 424
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+((y-(-0.8))-y-x)*((-x)*4.5*y/(-x))/((6.8+3.4+(-0.1))+(-y))-((-8.6)/(-3.0)/(-x)-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+((q-(-0.8))-q-p)*((-p)*4.5*q/(-p))/((6.8+3.4+(-0.1))+(-q))-((-8.6)/(-3.0)/(-p)-p))
            z2 <== (y+((y-(-0.8))-y-x)*((-x)*4.5*y/(-x))/((6.8+3.4+(-0.1))+(-y))-((-8.6)/(-3.0)/(-x)-x))
            wr.tt <| (I 424)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 425
        ctx.emit.comment "test425"
        //let z0 = 7.6
        //printfn "%d" <| 425
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 426
        ctx.emit.comment "test426"
        //let z0 = (((-x))+(6.0*(-x))+y/(-2.3)*(-y)-(-x)*(x+(-y)*y/(y)*x)*(-8.1))
        //printfn "%d" <| 426
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x))+(6.0*(-x))+y/(-2.3)*(-y)-(-x)*(x+(-y)*y/(y)*x)*(-8.1))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p))+(6.0*(-p))+q/(-2.3)*(-q)-(-p)*(p+(-q)*q/(q)*p)*(-8.1))
            z2 <== (((-x))+(6.0*(-x))+y/(-2.3)*(-y)-(-x)*(x+(-y)*y/(y)*x)*(-8.1))
            wr.tt <| (I 426)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 427
        ctx.emit.comment "test427"
        //let z0 = 7.7
        //printfn "%d" <| 427
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 428
        ctx.emit.comment "test428"
        //let z0 = (-x)
        //printfn "%d" <| 428
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 428)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 429
        ctx.emit.comment "test429"
        //let z0 = y
        //printfn "%d" <| 429
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 429)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 430
        ctx.emit.comment "test430"
        //let z0 = (-6.4)
        //printfn "%d" <| 430
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 431
        ctx.emit.comment "test431"
        //let z0 = y
        //printfn "%d" <| 431
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 431)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 432
        ctx.emit.comment "test432"
        //let z0 = (-7.1)
        //printfn "%d" <| 432
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 433
        ctx.emit.comment "test433"
        //let z0 = y
        //printfn "%d" <| 433
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 433)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 434
        ctx.emit.comment "test434"
        //let z0 = ((-y)*7.7+(-y))
        //printfn "%d" <| 434
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)*7.7+(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)*7.7+(-q))
            z2 <== ((-y)*7.7+(-y))
            wr.tt <| (I 434)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 435
        ctx.emit.comment "test435"
        //let z0 = ((-8.3))
        //printfn "%d" <| 435
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 436
        ctx.emit.comment "test436"
        //let z0 = (4.1/((-0.0)/y/(-y)*y-x)/(((-x))*x*(y-(-5.1)+(-x))*((-2.5)-(-7.7)*(-y)/x/(-8.5))-x))
        //printfn "%d" <| 436
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((4.1/((-0.0)/y/(-y)*y-x)/(((-x))*x*(y-(-5.1)+(-x))*((-2.5)-(-7.7)*(-y)/x/(-8.5))-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (4.1/((-0.0)/q/(-q)*q-p)/(((-p))*p*(q-(-5.1)+(-p))*((-2.5)-(-7.7)*(-q)/p/(-8.5))-p))
            z2 <== (4.1/((-0.0)/y/(-y)*y-x)/(((-x))*x*(y-(-5.1)+(-x))*((-2.5)-(-7.7)*(-y)/x/(-8.5))-x))
            wr.tt <| (I 436)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 437
        ctx.emit.comment "test437"
        //let z0 = (x/((-5.5))/0.8-y*y+(-x)*(-6.4)*((-4.2)+(-x))/y+y-(-y))
        //printfn "%d" <| 437
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x/((-5.5))/0.8-y*y+(-x)*(-6.4)*((-4.2)+(-x))/y+y-(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p/((-5.5))/0.8-q*q+(-p)*(-6.4)*((-4.2)+(-p))/q+q-(-q))
            z2 <== (x/((-5.5))/0.8-y*y+(-x)*(-6.4)*((-4.2)+(-x))/y+y-(-y))
            wr.tt <| (I 437)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 438
        ctx.emit.comment "test438"
        //let z0 = x
        //printfn "%d" <| 438
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 438)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 439
        ctx.emit.comment "test439"
        //let z0 = (5.2/y+(((-7.0)+0.7/6.6))*((-6.5)))
        //printfn "%d" <| 439
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.2/y+(((-7.0)+0.7/6.6))*((-6.5)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.2/q+(((-7.0)+0.7/6.6))*((-6.5)))
            z2 <== (5.2/y+(((-7.0)+0.7/6.6))*((-6.5)))
            wr.tt <| (I 439)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 440
        ctx.emit.comment "test440"
        //let z0 = (-1.4)
        //printfn "%d" <| 440
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 441
        ctx.emit.comment "test441"
        //let z0 = ((-y)-4.2+(x*2.0*(-4.8)*y*y*(-8.8)+(-4.4)))
        //printfn "%d" <| 441
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)-4.2+(x*2.0*(-4.8)*y*y*(-8.8)+(-4.4)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)-4.2+(p*2.0*(-4.8)*q*q*(-8.8)+(-4.4)))
            z2 <== ((-y)-4.2+(x*2.0*(-4.8)*y*y*(-8.8)+(-4.4)))
            wr.tt <| (I 441)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 442
        ctx.emit.comment "test442"
        //let z0 = 5.8
        //printfn "%d" <| 442
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 443
        ctx.emit.comment "test443"
        //let z0 = x
        //printfn "%d" <| 443
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 443)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 444
        ctx.emit.comment "test444"
        //let z0 = (-y)
        //printfn "%d" <| 444
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 444)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 445
        ctx.emit.comment "test445"
        //let z0 = (1.0/x-((-x)+(-y)*5.7-2.3+(-2.4))-3.6/(-y))
        //printfn "%d" <| 445
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((1.0/x-((-x)+(-y)*5.7-2.3+(-2.4))-3.6/(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (1.0/p-((-p)+(-q)*5.7-2.3+(-2.4))-3.6/(-q))
            z2 <== (1.0/x-((-x)+(-y)*5.7-2.3+(-2.4))-3.6/(-y))
            wr.tt <| (I 445)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 446
        ctx.emit.comment "test446"
        //let z0 = (((-7.6))*(0.6-x+x*x-3.6+7.5*((-4.8)/y+2.8)/x)/(-4.3)/y+(x/(-2.5)))
        //printfn "%d" <| 446
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-7.6))*(0.6-x+x*x-3.6+7.5*((-4.8)/y+2.8)/x)/(-4.3)/y+(x/(-2.5)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-7.6))*(0.6-p+p*p-3.6+7.5*((-4.8)/q+2.8)/p)/(-4.3)/q+(p/(-2.5)))
            z2 <== (((-7.6))*(0.6-x+x*x-3.6+7.5*((-4.8)/y+2.8)/x)/(-4.3)/y+(x/(-2.5)))
            wr.tt <| (I 446)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 447
        ctx.emit.comment "test447"
        //let z0 = ((-0.8)*(5.0))*((x/0.1+3.2)+(x/y)+((-1.4)+x)/x-(-1.6)*5.2-((-y)*(-7.3)+(-x)))
        //printfn "%d" <| 447
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.8)*(5.0))*((x/0.1+3.2)+(x/y)+((-1.4)+x)/x-(-1.6)*5.2-((-y)*(-7.3)+(-x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.8)*(5.0))*((p/0.1+3.2)+(p/q)+((-1.4)+p)/p-(-1.6)*5.2-((-q)*(-7.3)+(-p)))
            z2 <== ((-0.8)*(5.0))*((x/0.1+3.2)+(x/y)+((-1.4)+x)/x-(-1.6)*5.2-((-y)*(-7.3)+(-x)))
            wr.tt <| (I 447)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 448
        ctx.emit.comment "test448"
        //let z0 = (3.7-(-7.5)+(y-(-8.0)+(-4.3)-(-y)+x/x/y/(-7.7)+x)-(-3.1)-(((-4.4))*5.2/(x)*(-8.2)))
        //printfn "%d" <| 448
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((3.7-(-7.5)+(y-(-8.0)+(-4.3)-(-y)+x/x/y/(-7.7)+x)-(-3.1)-(((-4.4))*5.2/(x)*(-8.2)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (3.7-(-7.5)+(q-(-8.0)+(-4.3)-(-q)+p/p/q/(-7.7)+p)-(-3.1)-(((-4.4))*5.2/(p)*(-8.2)))
            z2 <== (3.7-(-7.5)+(y-(-8.0)+(-4.3)-(-y)+x/x/y/(-7.7)+x)-(-3.1)-(((-4.4))*5.2/(x)*(-8.2)))
            wr.tt <| (I 448)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 449
        ctx.emit.comment "test449"
        //let z0 = ((-y))
        //printfn "%d" <| 449
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q))
            z2 <== ((-y))
            wr.tt <| (I 449)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 450
        ctx.emit.comment "test450"
        //let z0 = 5.5
        //printfn "%d" <| 450
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 451
        ctx.emit.comment "test451"
        //let z0 = (6.3*(-3.0)-y*(-y)*(-y))
        //printfn "%d" <| 451
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((6.3*(-3.0)-y*(-y)*(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (6.3*(-3.0)-q*(-q)*(-q))
            z2 <== (6.3*(-3.0)-y*(-y)*(-y))
            wr.tt <| (I 451)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 452
        ctx.emit.comment "test452"
        //let z0 = (-y)
        //printfn "%d" <| 452
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 452)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 453
        ctx.emit.comment "test453"
        //let z0 = ((-x)-(-6.3)-(-x)/6.2/(-0.3)+8.8+((2.6+(-y)-x-(-2.8))*((-0.7)/2.7/0.1)-(-7.6)+((-x)*1.6)))
        //printfn "%d" <| 453
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)-(-6.3)-(-x)/6.2/(-0.3)+8.8+((2.6+(-y)-x-(-2.8))*((-0.7)/2.7/0.1)-(-7.6)+((-x)*1.6)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)-(-6.3)-(-p)/6.2/(-0.3)+8.8+((2.6+(-q)-p-(-2.8))*((-0.7)/2.7/0.1)-(-7.6)+((-p)*1.6)))
            z2 <== ((-x)-(-6.3)-(-x)/6.2/(-0.3)+8.8+((2.6+(-y)-x-(-2.8))*((-0.7)/2.7/0.1)-(-7.6)+((-x)*1.6)))
            wr.tt <| (I 453)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 454
        ctx.emit.comment "test454"
        //let z0 = (-y)
        //printfn "%d" <| 454
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 454)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 455
        ctx.emit.comment "test455"
        //let z0 = (-4.7)*(-7.7)-(-8.2)
        //printfn "%d" <| 455
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 456
        ctx.emit.comment "test456"
        //let z0 = ((((-8.6)/y-(-5.1)-(-5.5)+(-7.5))+(-0.2)/((-2.7)*(-2.4)/1.8*0.4*(-5.4))*(y*y+y/x)-(-8.2))/((x*7.0+x)*((-x)+0.1*(-x)*0.1)))
        //printfn "%d" <| 456
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-8.6)/y-(-5.1)-(-5.5)+(-7.5))+(-0.2)/((-2.7)*(-2.4)/1.8*0.4*(-5.4))*(y*y+y/x)-(-8.2))/((x*7.0+x)*((-x)+0.1*(-x)*0.1)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-8.6)/q-(-5.1)-(-5.5)+(-7.5))+(-0.2)/((-2.7)*(-2.4)/1.8*0.4*(-5.4))*(q*q+q/p)-(-8.2))/((p*7.0+p)*((-p)+0.1*(-p)*0.1)))
            z2 <== ((((-8.6)/y-(-5.1)-(-5.5)+(-7.5))+(-0.2)/((-2.7)*(-2.4)/1.8*0.4*(-5.4))*(y*y+y/x)-(-8.2))/((x*7.0+x)*((-x)+0.1*(-x)*0.1)))
            wr.tt <| (I 456)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 457
        ctx.emit.comment "test457"
        //let z0 = x
        //printfn "%d" <| 457
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 457)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 458
        ctx.emit.comment "test458"
        //let z0 = (x-(-y)-((-3.2)+4.8/x)*x)
        //printfn "%d" <| 458
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x-(-y)-((-3.2)+4.8/x)*x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p-(-q)-((-3.2)+4.8/p)*p)
            z2 <== (x-(-y)-((-3.2)+4.8/x)*x)
            wr.tt <| (I 458)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 459
        ctx.emit.comment "test459"
        //let z0 = (-y)
        //printfn "%d" <| 459
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 459)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 460
        ctx.emit.comment "test460"
        //let z0 = (-7.5)
        //printfn "%d" <| 460
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 461
        ctx.emit.comment "test461"
        //let z0 = 0.4*((0.6*(-8.6)+0.6+(-x))/5.5)-(-0.7)
        //printfn "%d" <| 461
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (0.4*((0.6*(-8.6)+0.6+(-x))/5.5)-(-0.7)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 0.4*((0.6*(-8.6)+0.6+(-p))/5.5)-(-0.7)
            z2 <== 0.4*((0.6*(-8.6)+0.6+(-x))/5.5)-(-0.7)
            wr.tt <| (I 461)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 462
        ctx.emit.comment "test462"
        //let z0 = (4.3*x*((-y)*(-y)*(-x)))*(((-2.0)*(-7.8)+4.6-2.5/y))/(-y)
        //printfn "%d" <| 462
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((4.3*x*((-y)*(-y)*(-x)))*(((-2.0)*(-7.8)+4.6-2.5/y))/(-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (4.3*p*((-q)*(-q)*(-p)))*(((-2.0)*(-7.8)+4.6-2.5/q))/(-q)
            z2 <== (4.3*x*((-y)*(-y)*(-x)))*(((-2.0)*(-7.8)+4.6-2.5/y))/(-y)
            wr.tt <| (I 462)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 463
        ctx.emit.comment "test463"
        //let z0 = (8.5+7.1)
        //printfn "%d" <| 463
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 464
        ctx.emit.comment "test464"
        //let z0 = (((-7.5)+(-4.5)*(-0.1)*0.4)+((-0.5))/(x)-((-x))-y+(1.2-5.8+4.0-(-0.5)*(-0.6)/7.6/(x*(-x)/x-(-5.1)))-((7.8*2.2+7.7)-y-(-8.6)+7.4-(-2.8)))
        //printfn "%d" <| 464
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-7.5)+(-4.5)*(-0.1)*0.4)+((-0.5))/(x)-((-x))-y+(1.2-5.8+4.0-(-0.5)*(-0.6)/7.6/(x*(-x)/x-(-5.1)))-((7.8*2.2+7.7)-y-(-8.6)+7.4-(-2.8)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-7.5)+(-4.5)*(-0.1)*0.4)+((-0.5))/(p)-((-p))-q+(1.2-5.8+4.0-(-0.5)*(-0.6)/7.6/(p*(-p)/p-(-5.1)))-((7.8*2.2+7.7)-q-(-8.6)+7.4-(-2.8)))
            z2 <== (((-7.5)+(-4.5)*(-0.1)*0.4)+((-0.5))/(x)-((-x))-y+(1.2-5.8+4.0-(-0.5)*(-0.6)/7.6/(x*(-x)/x-(-5.1)))-((7.8*2.2+7.7)-y-(-8.6)+7.4-(-2.8)))
            wr.tt <| (I 464)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 465
        ctx.emit.comment "test465"
        //let z0 = 5.1
        //printfn "%d" <| 465
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 466
        ctx.emit.comment "test466"
        //let z0 = 6.3
        //printfn "%d" <| 466
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 467
        ctx.emit.comment "test467"
        //let z0 = (y*0.7+(-x)*((4.1/3.0*(-y)+(-x))/2.2/x-(-y)-(-x)/(-0.0)-((-x)*6.5*x)))
        //printfn "%d" <| 467
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y*0.7+(-x)*((4.1/3.0*(-y)+(-x))/2.2/x-(-y)-(-x)/(-0.0)-((-x)*6.5*x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q*0.7+(-p)*((4.1/3.0*(-q)+(-p))/2.2/p-(-q)-(-p)/(-0.0)-((-p)*6.5*p)))
            z2 <== (y*0.7+(-x)*((4.1/3.0*(-y)+(-x))/2.2/x-(-y)-(-x)/(-0.0)-((-x)*6.5*x)))
            wr.tt <| (I 467)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 468
        ctx.emit.comment "test468"
        //let z0 = (-y)
        //printfn "%d" <| 468
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 468)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 469
        ctx.emit.comment "test469"
        //let z0 = ((-x)+(-x))
        //printfn "%d" <| 469
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+(-p))
            z2 <== ((-x)+(-x))
            wr.tt <| (I 469)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 470
        ctx.emit.comment "test470"
        //let z0 = (-y)
        //printfn "%d" <| 470
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 470)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 471
        ctx.emit.comment "test471"
        //let z0 = ((((-x)-(-y))+(x-(-6.2)*3.7)/(-y))*(x/(-y))/3.8/(((-8.3)+x-x)+((-6.4)*(-y)))+(-x))
        //printfn "%d" <| 471
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-x)-(-y))+(x-(-6.2)*3.7)/(-y))*(x/(-y))/3.8/(((-8.3)+x-x)+((-6.4)*(-y)))+(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-p)-(-q))+(p-(-6.2)*3.7)/(-q))*(p/(-q))/3.8/(((-8.3)+p-p)+((-6.4)*(-q)))+(-p))
            z2 <== ((((-x)-(-y))+(x-(-6.2)*3.7)/(-y))*(x/(-y))/3.8/(((-8.3)+x-x)+((-6.4)*(-y)))+(-x))
            wr.tt <| (I 471)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 472
        ctx.emit.comment "test472"
        //let z0 = 8.5
        //printfn "%d" <| 472
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 473
        ctx.emit.comment "test473"
        //let z0 = (-y)
        //printfn "%d" <| 473
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 473)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 474
        ctx.emit.comment "test474"
        //let z0 = 2.1*8.7/(-4.4)/(8.5)-8.3
        //printfn "%d" <| 474
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 475
        ctx.emit.comment "test475"
        //let z0 = (x*y*8.4/((-0.0)/4.0*(-2.6)*x/(-0.0))/((-y)/(-8.2)-y*(-5.6)))
        //printfn "%d" <| 475
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x*y*8.4/((-0.0)/4.0*(-2.6)*x/(-0.0))/((-y)/(-8.2)-y*(-5.6)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p*q*8.4/((-0.0)/4.0*(-2.6)*p/(-0.0))/((-q)/(-8.2)-q*(-5.6)))
            z2 <== (x*y*8.4/((-0.0)/4.0*(-2.6)*x/(-0.0))/((-y)/(-8.2)-y*(-5.6)))
            wr.tt <| (I 475)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 476
        ctx.emit.comment "test476"
        //let z0 = (-x)*((2.0*(-1.8)+4.4)/1.8)-(-x)
        //printfn "%d" <| 476
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)*((2.0*(-1.8)+4.4)/1.8)-(-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)*((2.0*(-1.8)+4.4)/1.8)-(-p)
            z2 <== (-x)*((2.0*(-1.8)+4.4)/1.8)-(-x)
            wr.tt <| (I 476)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 477
        ctx.emit.comment "test477"
        //let z0 = ((-x)+(-8.3))
        //printfn "%d" <| 477
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+(-8.3))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+(-8.3))
            z2 <== ((-x)+(-8.3))
            wr.tt <| (I 477)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 478
        ctx.emit.comment "test478"
        //let z0 = x+8.7/((-3.6))+((-y))+((-3.0)-2.5+x)-((-7.1)*8.1)/(-2.2)
        //printfn "%d" <| 478
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x+8.7/((-3.6))+((-y))+((-3.0)-2.5+x)-((-7.1)*8.1)/(-2.2)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p+8.7/((-3.6))+((-q))+((-3.0)-2.5+p)-((-7.1)*8.1)/(-2.2)
            z2 <== x+8.7/((-3.6))+((-y))+((-3.0)-2.5+x)-((-7.1)*8.1)/(-2.2)
            wr.tt <| (I 478)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 479
        ctx.emit.comment "test479"
        //let z0 = (-y)+(-7.3)*(-x)*(-1.2)-x/((0.8-6.3-8.7*(-y)+6.4)/(y*x)/5.3)+(-x)
        //printfn "%d" <| 479
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)+(-7.3)*(-x)*(-1.2)-x/((0.8-6.3-8.7*(-y)+6.4)/(y*x)/5.3)+(-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)+(-7.3)*(-p)*(-1.2)-p/((0.8-6.3-8.7*(-q)+6.4)/(q*p)/5.3)+(-p)
            z2 <== (-y)+(-7.3)*(-x)*(-1.2)-x/((0.8-6.3-8.7*(-y)+6.4)/(y*x)/5.3)+(-x)
            wr.tt <| (I 479)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 480
        ctx.emit.comment "test480"
        //let z0 = (y*(1.7+y/(-x))*0.2/(-y))/7.7-8.3/(-8.5)-(-y)
        //printfn "%d" <| 480
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y*(1.7+y/(-x))*0.2/(-y))/7.7-8.3/(-8.5)-(-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q*(1.7+q/(-p))*0.2/(-q))/7.7-8.3/(-8.5)-(-q)
            z2 <== (y*(1.7+y/(-x))*0.2/(-y))/7.7-8.3/(-8.5)-(-y)
            wr.tt <| (I 480)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 481
        ctx.emit.comment "test481"
        //let z0 = x
        //printfn "%d" <| 481
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 481)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 482
        ctx.emit.comment "test482"
        //let z0 = (-y)
        //printfn "%d" <| 482
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 482)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 483
        ctx.emit.comment "test483"
        //let z0 = (x/(((-x)*x+0.4))+y+2.1-(5.0))
        //printfn "%d" <| 483
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x/(((-x)*x+0.4))+y+2.1-(5.0))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p/(((-p)*p+0.4))+q+2.1-(5.0))
            z2 <== (x/(((-x)*x+0.4))+y+2.1-(5.0))
            wr.tt <| (I 483)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 484
        ctx.emit.comment "test484"
        //let z0 = (-y)/(y+(-6.8)+7.1*((-y)+(-6.6)-(-7.5)+(-x)*(-0.7))-(-1.6)-(-6.6))+(-x)
        //printfn "%d" <| 484
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)/(y+(-6.8)+7.1*((-y)+(-6.6)-(-7.5)+(-x)*(-0.7))-(-1.6)-(-6.6))+(-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)/(q+(-6.8)+7.1*((-q)+(-6.6)-(-7.5)+(-p)*(-0.7))-(-1.6)-(-6.6))+(-p)
            z2 <== (-y)/(y+(-6.8)+7.1*((-y)+(-6.6)-(-7.5)+(-x)*(-0.7))-(-1.6)-(-6.6))+(-x)
            wr.tt <| (I 484)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 485
        ctx.emit.comment "test485"
        //let z0 = (5.0*x*(y*(-x))+(x-(-x))+(6.1)-y/(-x)*(-y)-(-x))
        //printfn "%d" <| 485
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.0*x*(y*(-x))+(x-(-x))+(6.1)-y/(-x)*(-y)-(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.0*p*(q*(-p))+(p-(-p))+(6.1)-q/(-p)*(-q)-(-p))
            z2 <== (5.0*x*(y*(-x))+(x-(-x))+(6.1)-y/(-x)*(-y)-(-x))
            wr.tt <| (I 485)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 486
        ctx.emit.comment "test486"
        //let z0 = (((x)/((-y)+(-y)*7.0)))
        //printfn "%d" <| 486
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((x)/((-y)+(-y)*7.0)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((p)/((-q)+(-q)*7.0)))
            z2 <== (((x)/((-y)+(-y)*7.0)))
            wr.tt <| (I 486)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 487
        ctx.emit.comment "test487"
        //let z0 = 7.7
        //printfn "%d" <| 487
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 488
        ctx.emit.comment "test488"
        //let z0 = x
        //printfn "%d" <| 488
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 488)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 489
        ctx.emit.comment "test489"
        //let z0 = (-x)
        //printfn "%d" <| 489
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 489)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 490
        ctx.emit.comment "test490"
        //let z0 = (-y)
        //printfn "%d" <| 490
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 490)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 491
        ctx.emit.comment "test491"
        //let z0 = (-4.7)
        //printfn "%d" <| 491
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 492
        ctx.emit.comment "test492"
        //let z0 = (4.8)
        //printfn "%d" <| 492
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 493
        ctx.emit.comment "test493"
        //let z0 = y
        //printfn "%d" <| 493
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 493)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 494
        ctx.emit.comment "test494"
        //let z0 = (6.7)
        //printfn "%d" <| 494
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 495
        ctx.emit.comment "test495"
        //let z0 = (-x)
        //printfn "%d" <| 495
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 495)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 496
        ctx.emit.comment "test496"
        //let z0 = (0.4-7.8/(-y)*((-8.2)+y+2.4/x)*x*8.4)
        //printfn "%d" <| 496
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((0.4-7.8/(-y)*((-8.2)+y+2.4/x)*x*8.4)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (0.4-7.8/(-q)*((-8.2)+q+2.4/p)*p*8.4)
            z2 <== (0.4-7.8/(-y)*((-8.2)+y+2.4/x)*x*8.4)
            wr.tt <| (I 496)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 497
        ctx.emit.comment "test497"
        //let z0 = (-y)
        //printfn "%d" <| 497
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 497)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 498
        ctx.emit.comment "test498"
        //let z0 = x
        //printfn "%d" <| 498
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 498)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 499
        ctx.emit.comment "test499"
        //let z0 = (((-4.0)))
        //printfn "%d" <| 499
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 500
        ctx.emit.comment "test500"
        //let z0 = (y+(-8.7)/(y/0.3)*(-2.3)+(-y)-4.8)
        //printfn "%d" <| 500
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+(-8.7)/(y/0.3)*(-2.3)+(-y)-4.8)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+(-8.7)/(q/0.3)*(-2.3)+(-q)-4.8)
            z2 <== (y+(-8.7)/(y/0.3)*(-2.3)+(-y)-4.8)
            wr.tt <| (I 500)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 501
        ctx.emit.comment "test501"
        //let z0 = ((-x)+(-y))
        //printfn "%d" <| 501
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+(-q))
            z2 <== ((-x)+(-y))
            wr.tt <| (I 501)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 502
        ctx.emit.comment "test502"
        //let z0 = 7.6
        //printfn "%d" <| 502
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 503
        ctx.emit.comment "test503"
        //let z0 = (-y)
        //printfn "%d" <| 503
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 503)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 504
        ctx.emit.comment "test504"
        //let z0 = ((-1.6)-(x/(-4.4)*x/y+7.7)+y*(x+(-8.5)*(-y)*(-5.0)/(-x)))+((x)*(-y)-(0.3*6.2*(-7.3)+5.3)*(-x)+(-6.4)+x+(-x))-((-5.7)-3.5)
        //printfn "%d" <| 504
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-1.6)-(x/(-4.4)*x/y+7.7)+y*(x+(-8.5)*(-y)*(-5.0)/(-x)))+((x)*(-y)-(0.3*6.2*(-7.3)+5.3)*(-x)+(-6.4)+x+(-x))-((-5.7)-3.5)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-1.6)-(p/(-4.4)*p/q+7.7)+q*(p+(-8.5)*(-q)*(-5.0)/(-p)))+((p)*(-q)-(0.3*6.2*(-7.3)+5.3)*(-p)+(-6.4)+p+(-p))-((-5.7)-3.5)
            z2 <== ((-1.6)-(x/(-4.4)*x/y+7.7)+y*(x+(-8.5)*(-y)*(-5.0)/(-x)))+((x)*(-y)-(0.3*6.2*(-7.3)+5.3)*(-x)+(-6.4)+x+(-x))-((-5.7)-3.5)
            wr.tt <| (I 504)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 505
        ctx.emit.comment "test505"
        //let z0 = (y+(x-x-(-x)-((-y)-y)*x-(-y)-(-y))/x)
        //printfn "%d" <| 505
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+(x-x-(-x)-((-y)-y)*x-(-y)-(-y))/x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+(p-p-(-p)-((-q)-q)*p-(-q)-(-q))/p)
            z2 <== (y+(x-x-(-x)-((-y)-y)*x-(-y)-(-y))/x)
            wr.tt <| (I 505)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 506
        ctx.emit.comment "test506"
        //let z0 = ((-1.8)*((-y)*(-y)/(-5.7)*(5.7+(-y)+0.6/(-7.6)-(-8.5)))+(-0.1)*8.2+(-y)+x)
        //printfn "%d" <| 506
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-1.8)*((-y)*(-y)/(-5.7)*(5.7+(-y)+0.6/(-7.6)-(-8.5)))+(-0.1)*8.2+(-y)+x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-1.8)*((-q)*(-q)/(-5.7)*(5.7+(-q)+0.6/(-7.6)-(-8.5)))+(-0.1)*8.2+(-q)+p)
            z2 <== ((-1.8)*((-y)*(-y)/(-5.7)*(5.7+(-y)+0.6/(-7.6)-(-8.5)))+(-0.1)*8.2+(-y)+x)
            wr.tt <| (I 506)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 507
        ctx.emit.comment "test507"
        //let z0 = (-8.8)
        //printfn "%d" <| 507
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 508
        ctx.emit.comment "test508"
        //let z0 = ((x/3.4/0.5)+(-y)/(-1.5)-4.2+(-x)-3.2/6.8/(x+3.5+(-y)+2.0/(-0.0))+x/y)
        //printfn "%d" <| 508
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x/3.4/0.5)+(-y)/(-1.5)-4.2+(-x)-3.2/6.8/(x+3.5+(-y)+2.0/(-0.0))+x/y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p/3.4/0.5)+(-q)/(-1.5)-4.2+(-p)-3.2/6.8/(p+3.5+(-q)+2.0/(-0.0))+p/q)
            z2 <== ((x/3.4/0.5)+(-y)/(-1.5)-4.2+(-x)-3.2/6.8/(x+3.5+(-y)+2.0/(-0.0))+x/y)
            wr.tt <| (I 508)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 509
        ctx.emit.comment "test509"
        //let z0 = ((-0.4)-(6.0*(-y)/(-4.6)-(-y))+(x*(-6.7))+(1.3/(-5.6)+x)-(-y)-x)
        //printfn "%d" <| 509
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.4)-(6.0*(-y)/(-4.6)-(-y))+(x*(-6.7))+(1.3/(-5.6)+x)-(-y)-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.4)-(6.0*(-q)/(-4.6)-(-q))+(p*(-6.7))+(1.3/(-5.6)+p)-(-q)-p)
            z2 <== ((-0.4)-(6.0*(-y)/(-4.6)-(-y))+(x*(-6.7))+(1.3/(-5.6)+x)-(-y)-x)
            wr.tt <| (I 509)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 510
        ctx.emit.comment "test510"
        //let z0 = ((x-((-x)+(-7.7))))
        //printfn "%d" <| 510
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x-((-x)+(-7.7))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p-((-p)+(-7.7))))
            z2 <== ((x-((-x)+(-7.7))))
            wr.tt <| (I 510)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 511
        ctx.emit.comment "test511"
        //let z0 = ((-x)+x*(-4.7)/x/(-5.1)-(-y)-y-0.5*5.7)/((x/(-y))/5.3*(-6.0)*(-y)+(-y))-((3.1+y+2.3-x/4.5)/(x)+(-0.3)/5.4-(-x))
        //printfn "%d" <| 511
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+x*(-4.7)/x/(-5.1)-(-y)-y-0.5*5.7)/((x/(-y))/5.3*(-6.0)*(-y)+(-y))-((3.1+y+2.3-x/4.5)/(x)+(-0.3)/5.4-(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+p*(-4.7)/p/(-5.1)-(-q)-q-0.5*5.7)/((p/(-q))/5.3*(-6.0)*(-q)+(-q))-((3.1+q+2.3-p/4.5)/(p)+(-0.3)/5.4-(-p))
            z2 <== ((-x)+x*(-4.7)/x/(-5.1)-(-y)-y-0.5*5.7)/((x/(-y))/5.3*(-6.0)*(-y)+(-y))-((3.1+y+2.3-x/4.5)/(x)+(-0.3)/5.4-(-x))
            wr.tt <| (I 511)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 512
        ctx.emit.comment "test512"
        //let z0 = (-8.3)
        //printfn "%d" <| 512
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 513
        ctx.emit.comment "test513"
        //let z0 = ((5.4*y+(-y)*x*(-7.3)*((-x)/x)+((-3.5)+(-x)-(-y)*(-7.7)/7.0))/x)
        //printfn "%d" <| 513
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((5.4*y+(-y)*x*(-7.3)*((-x)/x)+((-3.5)+(-x)-(-y)*(-7.7)/7.0))/x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((5.4*q+(-q)*p*(-7.3)*((-p)/p)+((-3.5)+(-p)-(-q)*(-7.7)/7.0))/p)
            z2 <== ((5.4*y+(-y)*x*(-7.3)*((-x)/x)+((-3.5)+(-x)-(-y)*(-7.7)/7.0))/x)
            wr.tt <| (I 513)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 514
        ctx.emit.comment "test514"
        //let z0 = (x+((5.1+(-1.1)+y*(-y))+(-1.5))/(-y)*((-8.8)*((-4.8)+y/7.4-x-(-2.5))/(-x)/y)/(-3.3))
        //printfn "%d" <| 514
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x+((5.1+(-1.1)+y*(-y))+(-1.5))/(-y)*((-8.8)*((-4.8)+y/7.4-x-(-2.5))/(-x)/y)/(-3.3))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p+((5.1+(-1.1)+q*(-q))+(-1.5))/(-q)*((-8.8)*((-4.8)+q/7.4-p-(-2.5))/(-p)/q)/(-3.3))
            z2 <== (x+((5.1+(-1.1)+y*(-y))+(-1.5))/(-y)*((-8.8)*((-4.8)+y/7.4-x-(-2.5))/(-x)/y)/(-3.3))
            wr.tt <| (I 514)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 515
        ctx.emit.comment "test515"
        //let z0 = (((-5.6))+((-0.2)+y))
        //printfn "%d" <| 515
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-5.6))+((-0.2)+y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-5.6))+((-0.2)+q))
            z2 <== (((-5.6))+((-0.2)+y))
            wr.tt <| (I 515)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 516
        ctx.emit.comment "test516"
        //let z0 = 5.4/5.3*(y)+((-5.2)*(-7.3)*(-x))/x*(x)/(-x)*(3.7-(-y)+(-1.7)*1.8)-((-5.5)-7.3-y/1.7)*(-y)*(-2.7)*x*x/(-x)*(y/(y+y/7.7))
        //printfn "%d" <| 516
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (5.4/5.3*(y)+((-5.2)*(-7.3)*(-x))/x*(x)/(-x)*(3.7-(-y)+(-1.7)*1.8)-((-5.5)-7.3-y/1.7)*(-y)*(-2.7)*x*x/(-x)*(y/(y+y/7.7))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 5.4/5.3*(q)+((-5.2)*(-7.3)*(-p))/p*(p)/(-p)*(3.7-(-q)+(-1.7)*1.8)-((-5.5)-7.3-q/1.7)*(-q)*(-2.7)*p*p/(-p)*(q/(q+q/7.7))
            z2 <== 5.4/5.3*(y)+((-5.2)*(-7.3)*(-x))/x*(x)/(-x)*(3.7-(-y)+(-1.7)*1.8)-((-5.5)-7.3-y/1.7)*(-y)*(-2.7)*x*x/(-x)*(y/(y+y/7.7))
            wr.tt <| (I 516)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 517
        ctx.emit.comment "test517"
        //let z0 = y
        //printfn "%d" <| 517
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 517)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 518
        ctx.emit.comment "test518"
        //let z0 = 3.3
        //printfn "%d" <| 518
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 519
        ctx.emit.comment "test519"
        //let z0 = 0.4*(-x)
        //printfn "%d" <| 519
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (0.4*(-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 0.4*(-p)
            z2 <== 0.4*(-x)
            wr.tt <| (I 519)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 520
        ctx.emit.comment "test520"
        //let z0 = 2.8
        //printfn "%d" <| 520
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 521
        ctx.emit.comment "test521"
        //let z0 = 3.5
        //printfn "%d" <| 521
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 522
        ctx.emit.comment "test522"
        //let z0 = (8.2)
        //printfn "%d" <| 522
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 523
        ctx.emit.comment "test523"
        //let z0 = (((x-y)-(-2.0)*(y/(-1.0)))/y-(-x)*4.5-(((-x)-x+(-y))))
        //printfn "%d" <| 523
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((x-y)-(-2.0)*(y/(-1.0)))/y-(-x)*4.5-(((-x)-x+(-y))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((p-q)-(-2.0)*(q/(-1.0)))/q-(-p)*4.5-(((-p)-p+(-q))))
            z2 <== (((x-y)-(-2.0)*(y/(-1.0)))/y-(-x)*4.5-(((-x)-x+(-y))))
            wr.tt <| (I 523)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 524
        ctx.emit.comment "test524"
        //let z0 = ((((-3.8))-x-(0.0/0.2+8.6+2.2)-x))
        //printfn "%d" <| 524
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-3.8))-x-(0.0/0.2+8.6+2.2)-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-3.8))-p-(0.0/0.2+8.6+2.2)-p))
            z2 <== ((((-3.8))-x-(0.0/0.2+8.6+2.2)-x))
            wr.tt <| (I 524)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 525
        ctx.emit.comment "test525"
        //let z0 = x
        //printfn "%d" <| 525
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 525)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 526
        ctx.emit.comment "test526"
        //let z0 = 2.1
        //printfn "%d" <| 526
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 527
        ctx.emit.comment "test527"
        //let z0 = 6.5
        //printfn "%d" <| 527
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 528
        ctx.emit.comment "test528"
        //let z0 = (((-x))*2.0+(-x)*((-5.2)-y/(-y)/y+(-6.0))/y+x*6.6*x)
        //printfn "%d" <| 528
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x))*2.0+(-x)*((-5.2)-y/(-y)/y+(-6.0))/y+x*6.6*x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p))*2.0+(-p)*((-5.2)-q/(-q)/q+(-6.0))/q+p*6.6*p)
            z2 <== (((-x))*2.0+(-x)*((-5.2)-y/(-y)/y+(-6.0))/y+x*6.6*x)
            wr.tt <| (I 528)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 529
        ctx.emit.comment "test529"
        //let z0 = ((x-(-y)-(-6.5)*4.2/8.5))
        //printfn "%d" <| 529
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x-(-y)-(-6.5)*4.2/8.5))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p-(-q)-(-6.5)*4.2/8.5))
            z2 <== ((x-(-y)-(-6.5)*4.2/8.5))
            wr.tt <| (I 529)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 530
        ctx.emit.comment "test530"
        //let z0 = (-5.8)
        //printfn "%d" <| 530
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 531
        ctx.emit.comment "test531"
        //let z0 = x
        //printfn "%d" <| 531
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 531)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 532
        ctx.emit.comment "test532"
        //let z0 = (2.6)
        //printfn "%d" <| 532
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 533
        ctx.emit.comment "test533"
        //let z0 = (5.7)
        //printfn "%d" <| 533
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 534
        ctx.emit.comment "test534"
        //let z0 = (-y)-x*(4.8+x-y+(y))+2.8/8.2
        //printfn "%d" <| 534
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)-x*(4.8+x-y+(y))+2.8/8.2).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)-p*(4.8+p-q+(q))+2.8/8.2
            z2 <== (-y)-x*(4.8+x-y+(y))+2.8/8.2
            wr.tt <| (I 534)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 535
        ctx.emit.comment "test535"
        //let z0 = (-y)/(((-y)*(-y)+1.3/6.4)-(-8.2)+((-x)+(-y)*4.5)*(-7.6))-(-7.4)*(5.5/(-x)*(-1.2)/(-y)+4.6)+y-(-y)-y*(-5.1)
        //printfn "%d" <| 535
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)/(((-y)*(-y)+1.3/6.4)-(-8.2)+((-x)+(-y)*4.5)*(-7.6))-(-7.4)*(5.5/(-x)*(-1.2)/(-y)+4.6)+y-(-y)-y*(-5.1)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)/(((-q)*(-q)+1.3/6.4)-(-8.2)+((-p)+(-q)*4.5)*(-7.6))-(-7.4)*(5.5/(-p)*(-1.2)/(-q)+4.6)+q-(-q)-q*(-5.1)
            z2 <== (-y)/(((-y)*(-y)+1.3/6.4)-(-8.2)+((-x)+(-y)*4.5)*(-7.6))-(-7.4)*(5.5/(-x)*(-1.2)/(-y)+4.6)+y-(-y)-y*(-5.1)
            wr.tt <| (I 535)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 536
        ctx.emit.comment "test536"
        //let z0 = x
        //printfn "%d" <| 536
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 536)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 537
        ctx.emit.comment "test537"
        //let z0 = 4.7
        //printfn "%d" <| 537
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 538
        ctx.emit.comment "test538"
        //let z0 = (-y)
        //printfn "%d" <| 538
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 538)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 539
        ctx.emit.comment "test539"
        //let z0 = (-8.0)
        //printfn "%d" <| 539
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 540
        ctx.emit.comment "test540"
        //let z0 = (-0.1)
        //printfn "%d" <| 540
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 541
        ctx.emit.comment "test541"
        //let z0 = ((4.0+(x*y+(-1.7)*0.5+5.8)+8.5)/(1.7-(-x)+y)+8.0)
        //printfn "%d" <| 541
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((4.0+(x*y+(-1.7)*0.5+5.8)+8.5)/(1.7-(-x)+y)+8.0)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((4.0+(p*q+(-1.7)*0.5+5.8)+8.5)/(1.7-(-p)+q)+8.0)
            z2 <== ((4.0+(x*y+(-1.7)*0.5+5.8)+8.5)/(1.7-(-x)+y)+8.0)
            wr.tt <| (I 541)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 542
        ctx.emit.comment "test542"
        //let z0 = (x)
        //printfn "%d" <| 542
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p)
            z2 <== (x)
            wr.tt <| (I 542)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 543
        ctx.emit.comment "test543"
        //let z0 = x
        //printfn "%d" <| 543
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 543)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 544
        ctx.emit.comment "test544"
        //let z0 = 5.6
        //printfn "%d" <| 544
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 545
        ctx.emit.comment "test545"
        //let z0 = (-8.7)
        //printfn "%d" <| 545
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 546
        ctx.emit.comment "test546"
        //let z0 = ((-x)*(1.5*5.1/(-x)/(-y)))-6.7/6.1/((-x)+(-8.0)+(-y)*(-8.5)/x/(-3.8)/(-x)/6.7/x)
        //printfn "%d" <| 546
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)*(1.5*5.1/(-x)/(-y)))-6.7/6.1/((-x)+(-8.0)+(-y)*(-8.5)/x/(-3.8)/(-x)/6.7/x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)*(1.5*5.1/(-p)/(-q)))-6.7/6.1/((-p)+(-8.0)+(-q)*(-8.5)/p/(-3.8)/(-p)/6.7/p)
            z2 <== ((-x)*(1.5*5.1/(-x)/(-y)))-6.7/6.1/((-x)+(-8.0)+(-y)*(-8.5)/x/(-3.8)/(-x)/6.7/x)
            wr.tt <| (I 546)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 547
        ctx.emit.comment "test547"
        //let z0 = ((-0.5)/y/x/((-x)/(-8.8)/(-2.3)+x*(-7.7))*(y-(-1.1))*((-1.7)+(4.5/x)/y)+(x/(-2.8)-(-y))-y)
        //printfn "%d" <| 547
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.5)/y/x/((-x)/(-8.8)/(-2.3)+x*(-7.7))*(y-(-1.1))*((-1.7)+(4.5/x)/y)+(x/(-2.8)-(-y))-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.5)/q/p/((-p)/(-8.8)/(-2.3)+p*(-7.7))*(q-(-1.1))*((-1.7)+(4.5/p)/q)+(p/(-2.8)-(-q))-q)
            z2 <== ((-0.5)/y/x/((-x)/(-8.8)/(-2.3)+x*(-7.7))*(y-(-1.1))*((-1.7)+(4.5/x)/y)+(x/(-2.8)-(-y))-y)
            wr.tt <| (I 547)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 548
        ctx.emit.comment "test548"
        //let z0 = (-5.2)
        //printfn "%d" <| 548
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 549
        ctx.emit.comment "test549"
        //let z0 = (-8.1)
        //printfn "%d" <| 549
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 550
        ctx.emit.comment "test550"
        //let z0 = ((-y)/(-6.8))
        //printfn "%d" <| 550
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)/(-6.8))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)/(-6.8))
            z2 <== ((-y)/(-6.8))
            wr.tt <| (I 550)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 551
        ctx.emit.comment "test551"
        //let z0 = (((-x))*(2.2*((-y)*1.3-(-x)*x))+(((-y)/3.6-x)+0.3*(-5.7))-(x/(-1.0)/y*y*(-y)*3.6+(-x)+((-0.2)-x-4.2/x/(-1.6))+(3.2+(-x)+y)))
        //printfn "%d" <| 551
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x))*(2.2*((-y)*1.3-(-x)*x))+(((-y)/3.6-x)+0.3*(-5.7))-(x/(-1.0)/y*y*(-y)*3.6+(-x)+((-0.2)-x-4.2/x/(-1.6))+(3.2+(-x)+y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p))*(2.2*((-q)*1.3-(-p)*p))+(((-q)/3.6-p)+0.3*(-5.7))-(p/(-1.0)/q*q*(-q)*3.6+(-p)+((-0.2)-p-4.2/p/(-1.6))+(3.2+(-p)+q)))
            z2 <== (((-x))*(2.2*((-y)*1.3-(-x)*x))+(((-y)/3.6-x)+0.3*(-5.7))-(x/(-1.0)/y*y*(-y)*3.6+(-x)+((-0.2)-x-4.2/x/(-1.6))+(3.2+(-x)+y)))
            wr.tt <| (I 551)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 552
        ctx.emit.comment "test552"
        //let z0 = (x+((x+x+(-x))+x+4.7+0.8))
        //printfn "%d" <| 552
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x+((x+x+(-x))+x+4.7+0.8))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p+((p+p+(-p))+p+4.7+0.8))
            z2 <== (x+((x+x+(-x))+x+4.7+0.8))
            wr.tt <| (I 552)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 553
        ctx.emit.comment "test553"
        //let z0 = ((((-7.0)-x/(-y)/2.4+(-1.0))*6.3*0.7-6.7)*8.8*(-x))
        //printfn "%d" <| 553
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-7.0)-x/(-y)/2.4+(-1.0))*6.3*0.7-6.7)*8.8*(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-7.0)-p/(-q)/2.4+(-1.0))*6.3*0.7-6.7)*8.8*(-p))
            z2 <== ((((-7.0)-x/(-y)/2.4+(-1.0))*6.3*0.7-6.7)*8.8*(-x))
            wr.tt <| (I 553)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 554
        ctx.emit.comment "test554"
        //let z0 = (-y)
        //printfn "%d" <| 554
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 554)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 555
        ctx.emit.comment "test555"
        //let z0 = (-2.8)
        //printfn "%d" <| 555
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 556
        ctx.emit.comment "test556"
        //let z0 = (-x)
        //printfn "%d" <| 556
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 556)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 557
        ctx.emit.comment "test557"
        //let z0 = (-5.7)
        //printfn "%d" <| 557
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 558
        ctx.emit.comment "test558"
        //let z0 = 3.8/((-0.8)/y)-x+2.0-2.7/((-3.3)/(-3.8)-(-y)+7.8*(-y)-(3.4*(-1.6)*8.0))+(-x)
        //printfn "%d" <| 558
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (3.8/((-0.8)/y)-x+2.0-2.7/((-3.3)/(-3.8)-(-y)+7.8*(-y)-(3.4*(-1.6)*8.0))+(-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 3.8/((-0.8)/q)-p+2.0-2.7/((-3.3)/(-3.8)-(-q)+7.8*(-q)-(3.4*(-1.6)*8.0))+(-p)
            z2 <== 3.8/((-0.8)/y)-x+2.0-2.7/((-3.3)/(-3.8)-(-y)+7.8*(-y)-(3.4*(-1.6)*8.0))+(-x)
            wr.tt <| (I 558)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 559
        ctx.emit.comment "test559"
        //let z0 = 6.4
        //printfn "%d" <| 559
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 560
        ctx.emit.comment "test560"
        //let z0 = ((-x))
        //printfn "%d" <| 560
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p))
            z2 <== ((-x))
            wr.tt <| (I 560)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 561
        ctx.emit.comment "test561"
        //let z0 = (((-5.2)-((-y)-0.5-(-y)/5.1-(-1.1))))
        //printfn "%d" <| 561
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-5.2)-((-y)-0.5-(-y)/5.1-(-1.1))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-5.2)-((-q)-0.5-(-q)/5.1-(-1.1))))
            z2 <== (((-5.2)-((-y)-0.5-(-y)/5.1-(-1.1))))
            wr.tt <| (I 561)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 562
        ctx.emit.comment "test562"
        //let z0 = (-0.1)
        //printfn "%d" <| 562
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 563
        ctx.emit.comment "test563"
        //let z0 = ((-y)+8.0)
        //printfn "%d" <| 563
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)+8.0)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)+8.0)
            z2 <== ((-y)+8.0)
            wr.tt <| (I 563)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 564
        ctx.emit.comment "test564"
        //let z0 = x
        //printfn "%d" <| 564
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 564)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 565
        ctx.emit.comment "test565"
        //let z0 = ((-x)*5.6+((-1.8)-(-1.8)))*1.4/(7.1-((-x)+y)*(-x)+(-6.4)*6.2)
        //printfn "%d" <| 565
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)*5.6+((-1.8)-(-1.8)))*1.4/(7.1-((-x)+y)*(-x)+(-6.4)*6.2)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)*5.6+((-1.8)-(-1.8)))*1.4/(7.1-((-p)+q)*(-p)+(-6.4)*6.2)
            z2 <== ((-x)*5.6+((-1.8)-(-1.8)))*1.4/(7.1-((-x)+y)*(-x)+(-6.4)*6.2)
            wr.tt <| (I 565)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 566
        ctx.emit.comment "test566"
        //let z0 = x
        //printfn "%d" <| 566
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 566)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 567
        ctx.emit.comment "test567"
        //let z0 = ((5.8*8.8+(-x)-5.2-x)*(-1.8)*x-(0.8-3.7/1.7+(-0.6)*(y*(-1.0)+(-7.7))+(-5.0)/(4.3-(-x)-y*y))*(y*(-7.6)*(-6.2)+y/4.6))
        //printfn "%d" <| 567
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((5.8*8.8+(-x)-5.2-x)*(-1.8)*x-(0.8-3.7/1.7+(-0.6)*(y*(-1.0)+(-7.7))+(-5.0)/(4.3-(-x)-y*y))*(y*(-7.6)*(-6.2)+y/4.6))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((5.8*8.8+(-p)-5.2-p)*(-1.8)*p-(0.8-3.7/1.7+(-0.6)*(q*(-1.0)+(-7.7))+(-5.0)/(4.3-(-p)-q*q))*(q*(-7.6)*(-6.2)+q/4.6))
            z2 <== ((5.8*8.8+(-x)-5.2-x)*(-1.8)*x-(0.8-3.7/1.7+(-0.6)*(y*(-1.0)+(-7.7))+(-5.0)/(4.3-(-x)-y*y))*(y*(-7.6)*(-6.2)+y/4.6))
            wr.tt <| (I 567)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 568
        ctx.emit.comment "test568"
        //let z0 = (((-y))*(y)+2.0+y-2.2-(-y)*(-y)-(-x)+((-6.3)*(x+(-x)*(-3.4))+(y-4.7*y))+((y-5.0)+(6.7)/2.8*((-6.5))*(-3.2)))
        //printfn "%d" <| 568
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y))*(y)+2.0+y-2.2-(-y)*(-y)-(-x)+((-6.3)*(x+(-x)*(-3.4))+(y-4.7*y))+((y-5.0)+(6.7)/2.8*((-6.5))*(-3.2)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q))*(q)+2.0+q-2.2-(-q)*(-q)-(-p)+((-6.3)*(p+(-p)*(-3.4))+(q-4.7*q))+((q-5.0)+(6.7)/2.8*((-6.5))*(-3.2)))
            z2 <== (((-y))*(y)+2.0+y-2.2-(-y)*(-y)-(-x)+((-6.3)*(x+(-x)*(-3.4))+(y-4.7*y))+((y-5.0)+(6.7)/2.8*((-6.5))*(-3.2)))
            wr.tt <| (I 568)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 569
        ctx.emit.comment "test569"
        //let z0 = y
        //printfn "%d" <| 569
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 569)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 570
        ctx.emit.comment "test570"
        //let z0 = 6.4
        //printfn "%d" <| 570
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 571
        ctx.emit.comment "test571"
        //let z0 = (-x)
        //printfn "%d" <| 571
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 571)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 572
        ctx.emit.comment "test572"
        //let z0 = y/x-(-x)+0.6
        //printfn "%d" <| 572
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y/x-(-x)+0.6).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q/p-(-p)+0.6
            z2 <== y/x-(-x)+0.6
            wr.tt <| (I 572)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 573
        ctx.emit.comment "test573"
        //let z0 = y
        //printfn "%d" <| 573
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 573)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 574
        ctx.emit.comment "test574"
        //let z0 = (-x)
        //printfn "%d" <| 574
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 574)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 575
        ctx.emit.comment "test575"
        //let z0 = ((-2.6)/((x-x-(-8.5))*(-3.4))*(-8.1)*x-(-x)-(-1.2))
        //printfn "%d" <| 575
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-2.6)/((x-x-(-8.5))*(-3.4))*(-8.1)*x-(-x)-(-1.2))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-2.6)/((p-p-(-8.5))*(-3.4))*(-8.1)*p-(-p)-(-1.2))
            z2 <== ((-2.6)/((x-x-(-8.5))*(-3.4))*(-8.1)*x-(-x)-(-1.2))
            wr.tt <| (I 575)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 576
        ctx.emit.comment "test576"
        //let z0 = (-x)
        //printfn "%d" <| 576
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 576)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 577
        ctx.emit.comment "test577"
        //let z0 = x/y
        //printfn "%d" <| 577
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x/y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p/q
            z2 <== x/y
            wr.tt <| (I 577)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 578
        ctx.emit.comment "test578"
        //let z0 = y*(-y)+(((-3.1)-2.3/8.0+(-3.4)-y)-((-4.3)/(-0.3)*y/(-x)+(-x)))/(-x)-5.2
        //printfn "%d" <| 578
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y*(-y)+(((-3.1)-2.3/8.0+(-3.4)-y)-((-4.3)/(-0.3)*y/(-x)+(-x)))/(-x)-5.2).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q*(-q)+(((-3.1)-2.3/8.0+(-3.4)-q)-((-4.3)/(-0.3)*q/(-p)+(-p)))/(-p)-5.2
            z2 <== y*(-y)+(((-3.1)-2.3/8.0+(-3.4)-y)-((-4.3)/(-0.3)*y/(-x)+(-x)))/(-x)-5.2
            wr.tt <| (I 578)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 579
        ctx.emit.comment "test579"
        //let z0 = ((-y)*(y-(-x))*(-3.1)*((-y)-x))
        //printfn "%d" <| 579
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)*(y-(-x))*(-3.1)*((-y)-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)*(q-(-p))*(-3.1)*((-q)-p))
            z2 <== ((-y)*(y-(-x))*(-3.1)*((-y)-x))
            wr.tt <| (I 579)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 580
        ctx.emit.comment "test580"
        //let z0 = (((-y)*y/(6.1+(-4.6))*y-(x/(-6.6))))
        //printfn "%d" <| 580
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)*y/(6.1+(-4.6))*y-(x/(-6.6))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)*q/(6.1+(-4.6))*q-(p/(-6.6))))
            z2 <== (((-y)*y/(6.1+(-4.6))*y-(x/(-6.6))))
            wr.tt <| (I 580)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 581
        ctx.emit.comment "test581"
        //let z0 = 4.5
        //printfn "%d" <| 581
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 582
        ctx.emit.comment "test582"
        //let z0 = 3.6
        //printfn "%d" <| 582
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 583
        ctx.emit.comment "test583"
        //let z0 = (-y)
        //printfn "%d" <| 583
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 583)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 584
        ctx.emit.comment "test584"
        //let z0 = ((-y)-((x+(-x)))/((-x)/(-1.4)-x)/(-2.3))
        //printfn "%d" <| 584
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)-((x+(-x)))/((-x)/(-1.4)-x)/(-2.3))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)-((p+(-p)))/((-p)/(-1.4)-p)/(-2.3))
            z2 <== ((-y)-((x+(-x)))/((-x)/(-1.4)-x)/(-2.3))
            wr.tt <| (I 584)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 585
        ctx.emit.comment "test585"
        //let z0 = 8.6
        //printfn "%d" <| 585
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 586
        ctx.emit.comment "test586"
        //let z0 = ((7.5+((-2.2))/y-(y))*7.8*(8.8/2.2*(-y))+((-x)+(-2.2))-((y*5.2+y)))
        //printfn "%d" <| 586
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((7.5+((-2.2))/y-(y))*7.8*(8.8/2.2*(-y))+((-x)+(-2.2))-((y*5.2+y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((7.5+((-2.2))/q-(q))*7.8*(8.8/2.2*(-q))+((-p)+(-2.2))-((q*5.2+q)))
            z2 <== ((7.5+((-2.2))/y-(y))*7.8*(8.8/2.2*(-y))+((-x)+(-2.2))-((y*5.2+y)))
            wr.tt <| (I 586)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 587
        ctx.emit.comment "test587"
        //let z0 = 4.0
        //printfn "%d" <| 587
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 588
        ctx.emit.comment "test588"
        //let z0 = y
        //printfn "%d" <| 588
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 588)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 589
        ctx.emit.comment "test589"
        //let z0 = (y*(-x)-(2.2))
        //printfn "%d" <| 589
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y*(-x)-(2.2))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q*(-p)-(2.2))
            z2 <== (y*(-x)-(2.2))
            wr.tt <| (I 589)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 590
        ctx.emit.comment "test590"
        //let z0 = (-2.2)
        //printfn "%d" <| 590
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 591
        ctx.emit.comment "test591"
        //let z0 = ((-4.0)/(-x)*(3.7)+(x+y-(-4.3)-(-y)-x)-((-0.8)/(-7.5)-(-0.8)-(-y)/(-6.2)))+((-y)/0.8*(-0.7)+8.3)
        //printfn "%d" <| 591
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-4.0)/(-x)*(3.7)+(x+y-(-4.3)-(-y)-x)-((-0.8)/(-7.5)-(-0.8)-(-y)/(-6.2)))+((-y)/0.8*(-0.7)+8.3)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-4.0)/(-p)*(3.7)+(p+q-(-4.3)-(-q)-p)-((-0.8)/(-7.5)-(-0.8)-(-q)/(-6.2)))+((-q)/0.8*(-0.7)+8.3)
            z2 <== ((-4.0)/(-x)*(3.7)+(x+y-(-4.3)-(-y)-x)-((-0.8)/(-7.5)-(-0.8)-(-y)/(-6.2)))+((-y)/0.8*(-0.7)+8.3)
            wr.tt <| (I 591)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 592
        ctx.emit.comment "test592"
        //let z0 = (-8.3)
        //printfn "%d" <| 592
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 593
        ctx.emit.comment "test593"
        //let z0 = ((-y)+x*(-y)-(y/(-3.4)+(-x)+3.7)+(y/y/5.4)*((-y)*y*(-x))+y-((4.3*x/1.1)/1.8/((-y)-x))+((0.7+(-4.3))*((-x)/(-7.5))-(-y)/(-y)+5.7/x))
        //printfn "%d" <| 593
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)+x*(-y)-(y/(-3.4)+(-x)+3.7)+(y/y/5.4)*((-y)*y*(-x))+y-((4.3*x/1.1)/1.8/((-y)-x))+((0.7+(-4.3))*((-x)/(-7.5))-(-y)/(-y)+5.7/x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)+p*(-q)-(q/(-3.4)+(-p)+3.7)+(q/q/5.4)*((-q)*q*(-p))+q-((4.3*p/1.1)/1.8/((-q)-p))+((0.7+(-4.3))*((-p)/(-7.5))-(-q)/(-q)+5.7/p))
            z2 <== ((-y)+x*(-y)-(y/(-3.4)+(-x)+3.7)+(y/y/5.4)*((-y)*y*(-x))+y-((4.3*x/1.1)/1.8/((-y)-x))+((0.7+(-4.3))*((-x)/(-7.5))-(-y)/(-y)+5.7/x))
            wr.tt <| (I 593)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 594
        ctx.emit.comment "test594"
        //let z0 = (7.2*((-x)-(-y)+(-y)+1.4+(-y))-y/(-0.3)-6.5/(-7.4)-((-y)+4.6+x+(y*y)/3.5)-(6.7)+(-4.1))
        //printfn "%d" <| 594
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.2*((-x)-(-y)+(-y)+1.4+(-y))-y/(-0.3)-6.5/(-7.4)-((-y)+4.6+x+(y*y)/3.5)-(6.7)+(-4.1))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.2*((-p)-(-q)+(-q)+1.4+(-q))-q/(-0.3)-6.5/(-7.4)-((-q)+4.6+p+(q*q)/3.5)-(6.7)+(-4.1))
            z2 <== (7.2*((-x)-(-y)+(-y)+1.4+(-y))-y/(-0.3)-6.5/(-7.4)-((-y)+4.6+x+(y*y)/3.5)-(6.7)+(-4.1))
            wr.tt <| (I 594)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 595
        ctx.emit.comment "test595"
        //let z0 = (-y)-((-y)-(-7.4)/x*y*(-y))+((-x))-(-y)
        //printfn "%d" <| 595
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)-((-y)-(-7.4)/x*y*(-y))+((-x))-(-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)-((-q)-(-7.4)/p*q*(-q))+((-p))-(-q)
            z2 <== (-y)-((-y)-(-7.4)/x*y*(-y))+((-x))-(-y)
            wr.tt <| (I 595)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 596
        ctx.emit.comment "test596"
        //let z0 = y
        //printfn "%d" <| 596
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 596)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 597
        ctx.emit.comment "test597"
        //let z0 = (5.1)
        //printfn "%d" <| 597
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 598
        ctx.emit.comment "test598"
        //let z0 = (((-x)+((-y))+y*y/4.8)+6.3/((-1.0)/0.4))
        //printfn "%d" <| 598
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)+((-y))+y*y/4.8)+6.3/((-1.0)/0.4))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)+((-q))+q*q/4.8)+6.3/((-1.0)/0.4))
            z2 <== (((-x)+((-y))+y*y/4.8)+6.3/((-1.0)/0.4))
            wr.tt <| (I 598)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 599
        ctx.emit.comment "test599"
        //let z0 = (-3.8)
        //printfn "%d" <| 599
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 600
        ctx.emit.comment "test600"
        //let z0 = (((x-(-y)/(-0.5)/(-y))+y/(-x))-y-((3.4/(-8.2)-(-5.5)/(-0.2))))
        //printfn "%d" <| 600
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((x-(-y)/(-0.5)/(-y))+y/(-x))-y-((3.4/(-8.2)-(-5.5)/(-0.2))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((p-(-q)/(-0.5)/(-q))+q/(-p))-q-((3.4/(-8.2)-(-5.5)/(-0.2))))
            z2 <== (((x-(-y)/(-0.5)/(-y))+y/(-x))-y-((3.4/(-8.2)-(-5.5)/(-0.2))))
            wr.tt <| (I 600)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 601
        ctx.emit.comment "test601"
        //let z0 = (-y)
        //printfn "%d" <| 601
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 601)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 602
        ctx.emit.comment "test602"
        //let z0 = (((-x)*y+(-y))-(-y))
        //printfn "%d" <| 602
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)*y+(-y))-(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)*q+(-q))-(-q))
            z2 <== (((-x)*y+(-y))-(-y))
            wr.tt <| (I 602)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 603
        ctx.emit.comment "test603"
        //let z0 = (5.7/8.4*x)
        //printfn "%d" <| 603
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.7/8.4*x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.7/8.4*p)
            z2 <== (5.7/8.4*x)
            wr.tt <| (I 603)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 604
        ctx.emit.comment "test604"
        //let z0 = y+x+(-x)+5.3-(-8.7)*(-y)/(6.3+x-(-8.5))-8.5/8.7-((-5.4)-((-0.0)-(-x)-(-x)*(-x)))/2.7
        //printfn "%d" <| 604
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y+x+(-x)+5.3-(-8.7)*(-y)/(6.3+x-(-8.5))-8.5/8.7-((-5.4)-((-0.0)-(-x)-(-x)*(-x)))/2.7).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q+p+(-p)+5.3-(-8.7)*(-q)/(6.3+p-(-8.5))-8.5/8.7-((-5.4)-((-0.0)-(-p)-(-p)*(-p)))/2.7
            z2 <== y+x+(-x)+5.3-(-8.7)*(-y)/(6.3+x-(-8.5))-8.5/8.7-((-5.4)-((-0.0)-(-x)-(-x)*(-x)))/2.7
            wr.tt <| (I 604)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 605
        ctx.emit.comment "test605"
        //let z0 = 7.4
        //printfn "%d" <| 605
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 606
        ctx.emit.comment "test606"
        //let z0 = x
        //printfn "%d" <| 606
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 606)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 607
        ctx.emit.comment "test607"
        //let z0 = (-y)
        //printfn "%d" <| 607
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 607)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 608
        ctx.emit.comment "test608"
        //let z0 = ((y-(-0.4)-7.4/(-y)-y)*(-5.8))
        //printfn "%d" <| 608
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y-(-0.4)-7.4/(-y)-y)*(-5.8))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q-(-0.4)-7.4/(-q)-q)*(-5.8))
            z2 <== ((y-(-0.4)-7.4/(-y)-y)*(-5.8))
            wr.tt <| (I 608)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 609
        ctx.emit.comment "test609"
        //let z0 = ((-y)+0.5*((-y)+(-x)+(-8.7)+7.7)+(-0.5)+y*(-y)+6.4-(y+0.5+y/(-x)-y)*((-6.3)/8.5/x)*(-x))
        //printfn "%d" <| 609
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)+0.5*((-y)+(-x)+(-8.7)+7.7)+(-0.5)+y*(-y)+6.4-(y+0.5+y/(-x)-y)*((-6.3)/8.5/x)*(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)+0.5*((-q)+(-p)+(-8.7)+7.7)+(-0.5)+q*(-q)+6.4-(q+0.5+q/(-p)-q)*((-6.3)/8.5/p)*(-p))
            z2 <== ((-y)+0.5*((-y)+(-x)+(-8.7)+7.7)+(-0.5)+y*(-y)+6.4-(y+0.5+y/(-x)-y)*((-6.3)/8.5/x)*(-x))
            wr.tt <| (I 609)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 610
        ctx.emit.comment "test610"
        //let z0 = (-5.5)
        //printfn "%d" <| 610
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 611
        ctx.emit.comment "test611"
        //let z0 = 5.7
        //printfn "%d" <| 611
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 612
        ctx.emit.comment "test612"
        //let z0 = (y-6.8*(-0.0)*y*x-7.1/(6.1-3.2/(-2.7)))-(-x)/(((-0.0)/(-y)+(-y)/(-x)*7.8)/(-0.8)+(-x)+(-2.1)/7.3-(-6.3)-(-x)*(-0.6)-(-6.0))-((-2.0)/((-8.5)-5.0*1.1-(-5.0))/y)+y
        //printfn "%d" <| 612
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y-6.8*(-0.0)*y*x-7.1/(6.1-3.2/(-2.7)))-(-x)/(((-0.0)/(-y)+(-y)/(-x)*7.8)/(-0.8)+(-x)+(-2.1)/7.3-(-6.3)-(-x)*(-0.6)-(-6.0))-((-2.0)/((-8.5)-5.0*1.1-(-5.0))/y)+y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q-6.8*(-0.0)*q*p-7.1/(6.1-3.2/(-2.7)))-(-p)/(((-0.0)/(-q)+(-q)/(-p)*7.8)/(-0.8)+(-p)+(-2.1)/7.3-(-6.3)-(-p)*(-0.6)-(-6.0))-((-2.0)/((-8.5)-5.0*1.1-(-5.0))/q)+q
            z2 <== (y-6.8*(-0.0)*y*x-7.1/(6.1-3.2/(-2.7)))-(-x)/(((-0.0)/(-y)+(-y)/(-x)*7.8)/(-0.8)+(-x)+(-2.1)/7.3-(-6.3)-(-x)*(-0.6)-(-6.0))-((-2.0)/((-8.5)-5.0*1.1-(-5.0))/y)+y
            wr.tt <| (I 612)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 613
        ctx.emit.comment "test613"
        //let z0 = ((x+(-y))*(-x)/6.6-(-3.6)-(-5.6))
        //printfn "%d" <| 613
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x+(-y))*(-x)/6.6-(-3.6)-(-5.6))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p+(-q))*(-p)/6.6-(-3.6)-(-5.6))
            z2 <== ((x+(-y))*(-x)/6.6-(-3.6)-(-5.6))
            wr.tt <| (I 613)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 614
        ctx.emit.comment "test614"
        //let z0 = (-5.7)
        //printfn "%d" <| 614
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 615
        ctx.emit.comment "test615"
        //let z0 = (((-x)+(-0.3)*(-x)/((-x))))
        //printfn "%d" <| 615
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)+(-0.3)*(-x)/((-x))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)+(-0.3)*(-p)/((-p))))
            z2 <== (((-x)+(-0.3)*(-x)/((-x))))
            wr.tt <| (I 615)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 616
        ctx.emit.comment "test616"
        //let z0 = (7.5+(-x)-1.6)
        //printfn "%d" <| 616
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.5+(-x)-1.6)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.5+(-p)-1.6)
            z2 <== (7.5+(-x)-1.6)
            wr.tt <| (I 616)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 617
        ctx.emit.comment "test617"
        //let z0 = y
        //printfn "%d" <| 617
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 617)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 618
        ctx.emit.comment "test618"
        //let z0 = (y*(x/1.2+((-x)-x+(-x))))
        //printfn "%d" <| 618
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y*(x/1.2+((-x)-x+(-x))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q*(p/1.2+((-p)-p+(-p))))
            z2 <== (y*(x/1.2+((-x)-x+(-x))))
            wr.tt <| (I 618)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 619
        ctx.emit.comment "test619"
        //let z0 = y
        //printfn "%d" <| 619
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 619)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 620
        ctx.emit.comment "test620"
        //let z0 = (-x)
        //printfn "%d" <| 620
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 620)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 621
        ctx.emit.comment "test621"
        //let z0 = (y-(x/y+((-2.0)/(-y)*x+(-5.4)*(-y))/((-0.3)*(-x)+4.5))/(-y)*7.3/(y-(-6.1)-(-x)))
        //printfn "%d" <| 621
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y-(x/y+((-2.0)/(-y)*x+(-5.4)*(-y))/((-0.3)*(-x)+4.5))/(-y)*7.3/(y-(-6.1)-(-x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q-(p/q+((-2.0)/(-q)*p+(-5.4)*(-q))/((-0.3)*(-p)+4.5))/(-q)*7.3/(q-(-6.1)-(-p)))
            z2 <== (y-(x/y+((-2.0)/(-y)*x+(-5.4)*(-y))/((-0.3)*(-x)+4.5))/(-y)*7.3/(y-(-6.1)-(-x)))
            wr.tt <| (I 621)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 622
        ctx.emit.comment "test622"
        //let z0 = (-y)
        //printfn "%d" <| 622
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 622)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 623
        ctx.emit.comment "test623"
        //let z0 = (-2.0)
        //printfn "%d" <| 623
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 624
        ctx.emit.comment "test624"
        //let z0 = ((-4.4)*((x+0.3/y+(-y)-(-x))-(-y)-(y)/(-0.0)+((-2.3)/x))+(-x))
        //printfn "%d" <| 624
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-4.4)*((x+0.3/y+(-y)-(-x))-(-y)-(y)/(-0.0)+((-2.3)/x))+(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-4.4)*((p+0.3/q+(-q)-(-p))-(-q)-(q)/(-0.0)+((-2.3)/p))+(-p))
            z2 <== ((-4.4)*((x+0.3/y+(-y)-(-x))-(-y)-(y)/(-0.0)+((-2.3)/x))+(-x))
            wr.tt <| (I 624)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 625
        ctx.emit.comment "test625"
        //let z0 = x-(-x)/(-x)*0.0+((-x)-x+2.0*x)/(-8.4)+((-5.5))/x
        //printfn "%d" <| 625
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x-(-x)/(-x)*0.0+((-x)-x+2.0*x)/(-8.4)+((-5.5))/x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p-(-p)/(-p)*0.0+((-p)-p+2.0*p)/(-8.4)+((-5.5))/p
            z2 <== x-(-x)/(-x)*0.0+((-x)-x+2.0*x)/(-8.4)+((-5.5))/x
            wr.tt <| (I 625)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 626
        ctx.emit.comment "test626"
        //let z0 = ((8.4-((-y)*x+(-6.2)*(-x)))*(-x)+(y+(-7.5)/y*y)*(y)/x*(-4.3)+(-y)-(-3.8)/(-8.6)-1.6)
        //printfn "%d" <| 626
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((8.4-((-y)*x+(-6.2)*(-x)))*(-x)+(y+(-7.5)/y*y)*(y)/x*(-4.3)+(-y)-(-3.8)/(-8.6)-1.6)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((8.4-((-q)*p+(-6.2)*(-p)))*(-p)+(q+(-7.5)/q*q)*(q)/p*(-4.3)+(-q)-(-3.8)/(-8.6)-1.6)
            z2 <== ((8.4-((-y)*x+(-6.2)*(-x)))*(-x)+(y+(-7.5)/y*y)*(y)/x*(-4.3)+(-y)-(-3.8)/(-8.6)-1.6)
            wr.tt <| (I 626)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 627
        ctx.emit.comment "test627"
        //let z0 = ((x)+((-5.6))-(-4.4)*3.1)
        //printfn "%d" <| 627
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x)+((-5.6))-(-4.4)*3.1)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p)+((-5.6))-(-4.4)*3.1)
            z2 <== ((x)+((-5.6))-(-4.4)*3.1)
            wr.tt <| (I 627)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 628
        ctx.emit.comment "test628"
        //let z0 = x
        //printfn "%d" <| 628
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 628)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 629
        ctx.emit.comment "test629"
        //let z0 = (x)
        //printfn "%d" <| 629
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p)
            z2 <== (x)
            wr.tt <| (I 629)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 630
        ctx.emit.comment "test630"
        //let z0 = y
        //printfn "%d" <| 630
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 630)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 631
        ctx.emit.comment "test631"
        //let z0 = (((x+3.3*x+x))+((-x)/(2.5*4.3/(-y)-x+y))*((-2.0)+3.4+5.8-((-4.5)+2.8+(-7.4)/3.2*y))/((y*(-y)/(-8.2))+(-5.4)/(1.7)/((-4.2)-x)*(4.1-(-x)-6.4-5.8/(-y)))*(-x))
        //printfn "%d" <| 631
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((x+3.3*x+x))+((-x)/(2.5*4.3/(-y)-x+y))*((-2.0)+3.4+5.8-((-4.5)+2.8+(-7.4)/3.2*y))/((y*(-y)/(-8.2))+(-5.4)/(1.7)/((-4.2)-x)*(4.1-(-x)-6.4-5.8/(-y)))*(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((p+3.3*p+p))+((-p)/(2.5*4.3/(-q)-p+q))*((-2.0)+3.4+5.8-((-4.5)+2.8+(-7.4)/3.2*q))/((q*(-q)/(-8.2))+(-5.4)/(1.7)/((-4.2)-p)*(4.1-(-p)-6.4-5.8/(-q)))*(-p))
            z2 <== (((x+3.3*x+x))+((-x)/(2.5*4.3/(-y)-x+y))*((-2.0)+3.4+5.8-((-4.5)+2.8+(-7.4)/3.2*y))/((y*(-y)/(-8.2))+(-5.4)/(1.7)/((-4.2)-x)*(4.1-(-x)-6.4-5.8/(-y)))*(-x))
            wr.tt <| (I 631)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 632
        ctx.emit.comment "test632"
        //let z0 = 4.0/y
        //printfn "%d" <| 632
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (4.0/y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 4.0/q
            z2 <== 4.0/y
            wr.tt <| (I 632)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 633
        ctx.emit.comment "test633"
        //let z0 = (8.0/((0.1)-(x-(-x)-(-x)-5.8*(-y))-(-3.2)+(-x))+x/y+(-2.1)/3.6+(8.3/(-5.4)/(-2.0)*x/x)+6.0/(-6.0)/x-x)
        //printfn "%d" <| 633
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((8.0/((0.1)-(x-(-x)-(-x)-5.8*(-y))-(-3.2)+(-x))+x/y+(-2.1)/3.6+(8.3/(-5.4)/(-2.0)*x/x)+6.0/(-6.0)/x-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (8.0/((0.1)-(p-(-p)-(-p)-5.8*(-q))-(-3.2)+(-p))+p/q+(-2.1)/3.6+(8.3/(-5.4)/(-2.0)*p/p)+6.0/(-6.0)/p-p)
            z2 <== (8.0/((0.1)-(x-(-x)-(-x)-5.8*(-y))-(-3.2)+(-x))+x/y+(-2.1)/3.6+(8.3/(-5.4)/(-2.0)*x/x)+6.0/(-6.0)/x-x)
            wr.tt <| (I 633)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 634
        ctx.emit.comment "test634"
        //let z0 = (-1.5)
        //printfn "%d" <| 634
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 635
        ctx.emit.comment "test635"
        //let z0 = ((5.2+y/2.2/(-y))+(x-(-8.2)*7.5+(-y)+(-8.5))+(-6.7)/(-7.0)*2.3+(-x)-x-y+(-2.8))
        //printfn "%d" <| 635
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((5.2+y/2.2/(-y))+(x-(-8.2)*7.5+(-y)+(-8.5))+(-6.7)/(-7.0)*2.3+(-x)-x-y+(-2.8))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((5.2+q/2.2/(-q))+(p-(-8.2)*7.5+(-q)+(-8.5))+(-6.7)/(-7.0)*2.3+(-p)-p-q+(-2.8))
            z2 <== ((5.2+y/2.2/(-y))+(x-(-8.2)*7.5+(-y)+(-8.5))+(-6.7)/(-7.0)*2.3+(-x)-x-y+(-2.8))
            wr.tt <| (I 635)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 636
        ctx.emit.comment "test636"
        //let z0 = 1.3+x/(-2.1)*(-x)*y+(-x)/(-x)+(-x)+((0.1-(-x))-(y/x+(-8.4))/(-6.4)/((-y)))+(((-5.8)/(-6.4)-(-y))+(-x)/2.6*x/(-3.8)/((-y)/(-5.8))/(-y))
        //printfn "%d" <| 636
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (1.3+x/(-2.1)*(-x)*y+(-x)/(-x)+(-x)+((0.1-(-x))-(y/x+(-8.4))/(-6.4)/((-y)))+(((-5.8)/(-6.4)-(-y))+(-x)/2.6*x/(-3.8)/((-y)/(-5.8))/(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 1.3+p/(-2.1)*(-p)*q+(-p)/(-p)+(-p)+((0.1-(-p))-(q/p+(-8.4))/(-6.4)/((-q)))+(((-5.8)/(-6.4)-(-q))+(-p)/2.6*p/(-3.8)/((-q)/(-5.8))/(-q))
            z2 <== 1.3+x/(-2.1)*(-x)*y+(-x)/(-x)+(-x)+((0.1-(-x))-(y/x+(-8.4))/(-6.4)/((-y)))+(((-5.8)/(-6.4)-(-y))+(-x)/2.6*x/(-3.8)/((-y)/(-5.8))/(-y))
            wr.tt <| (I 636)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 637
        ctx.emit.comment "test637"
        //let z0 = y
        //printfn "%d" <| 637
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 637)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 638
        ctx.emit.comment "test638"
        //let z0 = (((-2.6)+(y+(-6.4)/(-5.6))+(6.8)*y/(-y))/((-6.7)/(-x)/(6.1-4.6-(-y)*x))+((y-x+(-y)))-(-y))
        //printfn "%d" <| 638
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-2.6)+(y+(-6.4)/(-5.6))+(6.8)*y/(-y))/((-6.7)/(-x)/(6.1-4.6-(-y)*x))+((y-x+(-y)))-(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-2.6)+(q+(-6.4)/(-5.6))+(6.8)*q/(-q))/((-6.7)/(-p)/(6.1-4.6-(-q)*p))+((q-p+(-q)))-(-q))
            z2 <== (((-2.6)+(y+(-6.4)/(-5.6))+(6.8)*y/(-y))/((-6.7)/(-x)/(6.1-4.6-(-y)*x))+((y-x+(-y)))-(-y))
            wr.tt <| (I 638)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 639
        ctx.emit.comment "test639"
        //let z0 = (5.6+y+((-0.4))-(2.3+3.5-(-x)*x-(-y))*(-5.8)+x)
        //printfn "%d" <| 639
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.6+y+((-0.4))-(2.3+3.5-(-x)*x-(-y))*(-5.8)+x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.6+q+((-0.4))-(2.3+3.5-(-p)*p-(-q))*(-5.8)+p)
            z2 <== (5.6+y+((-0.4))-(2.3+3.5-(-x)*x-(-y))*(-5.8)+x)
            wr.tt <| (I 639)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 640
        ctx.emit.comment "test640"
        //let z0 = ((x/(5.7/(-6.8)*(-6.2)+0.6*(-y)))*(-x)+((-x)/1.5-(-y)/(x-3.1-y/(-y))+((-y)/1.7*(-4.3))))
        //printfn "%d" <| 640
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x/(5.7/(-6.8)*(-6.2)+0.6*(-y)))*(-x)+((-x)/1.5-(-y)/(x-3.1-y/(-y))+((-y)/1.7*(-4.3))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p/(5.7/(-6.8)*(-6.2)+0.6*(-q)))*(-p)+((-p)/1.5-(-q)/(p-3.1-q/(-q))+((-q)/1.7*(-4.3))))
            z2 <== ((x/(5.7/(-6.8)*(-6.2)+0.6*(-y)))*(-x)+((-x)/1.5-(-y)/(x-3.1-y/(-y))+((-y)/1.7*(-4.3))))
            wr.tt <| (I 640)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 641
        ctx.emit.comment "test641"
        //let z0 = (-x)
        //printfn "%d" <| 641
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 641)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 642
        ctx.emit.comment "test642"
        //let z0 = 7.4
        //printfn "%d" <| 642
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 643
        ctx.emit.comment "test643"
        //let z0 = (4.2-y/(7.3-y/(-x))*(-8.8)-((-8.0)/y*y*(-y)))
        //printfn "%d" <| 643
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((4.2-y/(7.3-y/(-x))*(-8.8)-((-8.0)/y*y*(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (4.2-q/(7.3-q/(-p))*(-8.8)-((-8.0)/q*q*(-q)))
            z2 <== (4.2-y/(7.3-y/(-x))*(-8.8)-((-8.0)/y*y*(-y)))
            wr.tt <| (I 643)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 644
        ctx.emit.comment "test644"
        //let z0 = (3.4)
        //printfn "%d" <| 644
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 645
        ctx.emit.comment "test645"
        //let z0 = (((-4.0)*(x/x+(-y))-((-6.8)/(-y))*(x))-((x/8.3/(-y))))
        //printfn "%d" <| 645
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-4.0)*(x/x+(-y))-((-6.8)/(-y))*(x))-((x/8.3/(-y))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-4.0)*(p/p+(-q))-((-6.8)/(-q))*(p))-((p/8.3/(-q))))
            z2 <== (((-4.0)*(x/x+(-y))-((-6.8)/(-y))*(x))-((x/8.3/(-y))))
            wr.tt <| (I 645)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 646
        ctx.emit.comment "test646"
        //let z0 = (((8.3+(-0.3)*(-y))+y+(-x)*(-y)-3.0/(8.6/(-5.1)*x/(-y)))+((x-(-x))/y+((-y)+y/3.4*(-y)))+(-6.8))
        //printfn "%d" <| 646
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((8.3+(-0.3)*(-y))+y+(-x)*(-y)-3.0/(8.6/(-5.1)*x/(-y)))+((x-(-x))/y+((-y)+y/3.4*(-y)))+(-6.8))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((8.3+(-0.3)*(-q))+q+(-p)*(-q)-3.0/(8.6/(-5.1)*p/(-q)))+((p-(-p))/q+((-q)+q/3.4*(-q)))+(-6.8))
            z2 <== (((8.3+(-0.3)*(-y))+y+(-x)*(-y)-3.0/(8.6/(-5.1)*x/(-y)))+((x-(-x))/y+((-y)+y/3.4*(-y)))+(-6.8))
            wr.tt <| (I 646)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 647
        ctx.emit.comment "test647"
        //let z0 = y
        //printfn "%d" <| 647
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 647)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 648
        ctx.emit.comment "test648"
        //let z0 = (((-7.6)-1.5*x)*(-6.2))
        //printfn "%d" <| 648
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-7.6)-1.5*x)*(-6.2))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-7.6)-1.5*p)*(-6.2))
            z2 <== (((-7.6)-1.5*x)*(-6.2))
            wr.tt <| (I 648)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 649
        ctx.emit.comment "test649"
        //let z0 = 1.5
        //printfn "%d" <| 649
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 650
        ctx.emit.comment "test650"
        //let z0 = 6.5
        //printfn "%d" <| 650
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 651
        ctx.emit.comment "test651"
        //let z0 = 1.4
        //printfn "%d" <| 651
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 652
        ctx.emit.comment "test652"
        //let z0 = ((x-(-x)-(-5.1)+x/(-3.7))*(-x))/(x*y)/7.1+y+(-3.4)
        //printfn "%d" <| 652
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x-(-x)-(-5.1)+x/(-3.7))*(-x))/(x*y)/7.1+y+(-3.4)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p-(-p)-(-5.1)+p/(-3.7))*(-p))/(p*q)/7.1+q+(-3.4)
            z2 <== ((x-(-x)-(-5.1)+x/(-3.7))*(-x))/(x*y)/7.1+y+(-3.4)
            wr.tt <| (I 652)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 653
        ctx.emit.comment "test653"
        //let z0 = (-x)
        //printfn "%d" <| 653
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 653)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 654
        ctx.emit.comment "test654"
        //let z0 = (2.7)
        //printfn "%d" <| 654
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 655
        ctx.emit.comment "test655"
        //let z0 = (((5.0/(-1.0)+(-y)/(-y)/(-y))/(1.4+8.0)/((-0.2)+y*(-y))/(x*y+y)*(-y)+4.3+(-x)/(-7.4)-y)+(-2.3)/8.2/(5.6*(-x)/4.0+x)+(-1.5))
        //printfn "%d" <| 655
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((5.0/(-1.0)+(-y)/(-y)/(-y))/(1.4+8.0)/((-0.2)+y*(-y))/(x*y+y)*(-y)+4.3+(-x)/(-7.4)-y)+(-2.3)/8.2/(5.6*(-x)/4.0+x)+(-1.5))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((5.0/(-1.0)+(-q)/(-q)/(-q))/(1.4+8.0)/((-0.2)+q*(-q))/(p*q+q)*(-q)+4.3+(-p)/(-7.4)-q)+(-2.3)/8.2/(5.6*(-p)/4.0+p)+(-1.5))
            z2 <== (((5.0/(-1.0)+(-y)/(-y)/(-y))/(1.4+8.0)/((-0.2)+y*(-y))/(x*y+y)*(-y)+4.3+(-x)/(-7.4)-y)+(-2.3)/8.2/(5.6*(-x)/4.0+x)+(-1.5))
            wr.tt <| (I 655)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 656
        ctx.emit.comment "test656"
        //let z0 = 2.8
        //printfn "%d" <| 656
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 657
        ctx.emit.comment "test657"
        //let z0 = (((y*(-0.3)-7.1)/(-5.2)+(-7.8)+(x)*((-3.0)/y*(-x)))+6.4*7.6)
        //printfn "%d" <| 657
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((y*(-0.3)-7.1)/(-5.2)+(-7.8)+(x)*((-3.0)/y*(-x)))+6.4*7.6)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((q*(-0.3)-7.1)/(-5.2)+(-7.8)+(p)*((-3.0)/q*(-p)))+6.4*7.6)
            z2 <== (((y*(-0.3)-7.1)/(-5.2)+(-7.8)+(x)*((-3.0)/y*(-x)))+6.4*7.6)
            wr.tt <| (I 657)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 658
        ctx.emit.comment "test658"
        //let z0 = (2.4*(2.6-(-0.1))*(-2.0))
        //printfn "%d" <| 658
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 659
        ctx.emit.comment "test659"
        //let z0 = (((-5.7)+(x*7.4-4.1)))
        //printfn "%d" <| 659
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-5.7)+(x*7.4-4.1)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-5.7)+(p*7.4-4.1)))
            z2 <== (((-5.7)+(x*7.4-4.1)))
            wr.tt <| (I 659)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 660
        ctx.emit.comment "test660"
        //let z0 = y
        //printfn "%d" <| 660
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 660)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 661
        ctx.emit.comment "test661"
        //let z0 = 7.4
        //printfn "%d" <| 661
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 662
        ctx.emit.comment "test662"
        //let z0 = ((8.4-3.4+(8.8/y+x)-((-0.1)+y*(-x)-(-y)/y)-y)*(4.7-((-1.3)-1.5-y-1.4/(-6.7))*((-0.6)+(-2.5)+(-x)))-(-x)+((-y)+(-x)-y/(y*(-7.0))*(-y)))
        //printfn "%d" <| 662
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((8.4-3.4+(8.8/y+x)-((-0.1)+y*(-x)-(-y)/y)-y)*(4.7-((-1.3)-1.5-y-1.4/(-6.7))*((-0.6)+(-2.5)+(-x)))-(-x)+((-y)+(-x)-y/(y*(-7.0))*(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((8.4-3.4+(8.8/q+p)-((-0.1)+q*(-p)-(-q)/q)-q)*(4.7-((-1.3)-1.5-q-1.4/(-6.7))*((-0.6)+(-2.5)+(-p)))-(-p)+((-q)+(-p)-q/(q*(-7.0))*(-q)))
            z2 <== ((8.4-3.4+(8.8/y+x)-((-0.1)+y*(-x)-(-y)/y)-y)*(4.7-((-1.3)-1.5-y-1.4/(-6.7))*((-0.6)+(-2.5)+(-x)))-(-x)+((-y)+(-x)-y/(y*(-7.0))*(-y)))
            wr.tt <| (I 662)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 663
        ctx.emit.comment "test663"
        //let z0 = (-7.0)
        //printfn "%d" <| 663
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 664
        ctx.emit.comment "test664"
        //let z0 = ((-y)-(y/x))
        //printfn "%d" <| 664
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)-(y/x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)-(q/p))
            z2 <== ((-y)-(y/x))
            wr.tt <| (I 664)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 665
        ctx.emit.comment "test665"
        //let z0 = 1.1
        //printfn "%d" <| 665
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 666
        ctx.emit.comment "test666"
        //let z0 = (-6.7)*(-1.2)+(((-0.5)-(-y))+0.0*(-y)*(x*8.5+x)/(0.6+(-y)))*x
        //printfn "%d" <| 666
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-6.7)*(-1.2)+(((-0.5)-(-y))+0.0*(-y)*(x*8.5+x)/(0.6+(-y)))*x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-6.7)*(-1.2)+(((-0.5)-(-q))+0.0*(-q)*(p*8.5+p)/(0.6+(-q)))*p
            z2 <== (-6.7)*(-1.2)+(((-0.5)-(-y))+0.0*(-y)*(x*8.5+x)/(0.6+(-y)))*x
            wr.tt <| (I 666)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 667
        ctx.emit.comment "test667"
        //let z0 = (((-4.1)-7.8))
        //printfn "%d" <| 667
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 668
        ctx.emit.comment "test668"
        //let z0 = ((2.8-x-(-x)))
        //printfn "%d" <| 668
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((2.8-x-(-x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((2.8-p-(-p)))
            z2 <== ((2.8-x-(-x)))
            wr.tt <| (I 668)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 669
        ctx.emit.comment "test669"
        //let z0 = (((-1.0)*((-8.5)*8.1)+((-5.2)/6.0-4.0/y/y))-5.1)
        //printfn "%d" <| 669
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-1.0)*((-8.5)*8.1)+((-5.2)/6.0-4.0/y/y))-5.1)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-1.0)*((-8.5)*8.1)+((-5.2)/6.0-4.0/q/q))-5.1)
            z2 <== (((-1.0)*((-8.5)*8.1)+((-5.2)/6.0-4.0/y/y))-5.1)
            wr.tt <| (I 669)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 670
        ctx.emit.comment "test670"
        //let z0 = y
        //printfn "%d" <| 670
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 670)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 671
        ctx.emit.comment "test671"
        //let z0 = ((-x)*(-4.2)*y-((y+(-3.1))-4.1+(-y)/(-1.8)+3.7)*(-6.0)*(-1.3))
        //printfn "%d" <| 671
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)*(-4.2)*y-((y+(-3.1))-4.1+(-y)/(-1.8)+3.7)*(-6.0)*(-1.3))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)*(-4.2)*q-((q+(-3.1))-4.1+(-q)/(-1.8)+3.7)*(-6.0)*(-1.3))
            z2 <== ((-x)*(-4.2)*y-((y+(-3.1))-4.1+(-y)/(-1.8)+3.7)*(-6.0)*(-1.3))
            wr.tt <| (I 671)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 672
        ctx.emit.comment "test672"
        //let z0 = (-0.1)
        //printfn "%d" <| 672
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 673
        ctx.emit.comment "test673"
        //let z0 = 3.1
        //printfn "%d" <| 673
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 674
        ctx.emit.comment "test674"
        //let z0 = x
        //printfn "%d" <| 674
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 674)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 675
        ctx.emit.comment "test675"
        //let z0 = 6.4
        //printfn "%d" <| 675
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 676
        ctx.emit.comment "test676"
        //let z0 = y
        //printfn "%d" <| 676
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 676)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 677
        ctx.emit.comment "test677"
        //let z0 = (-y)
        //printfn "%d" <| 677
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 677)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 678
        ctx.emit.comment "test678"
        //let z0 = ((((-x)*(-5.4)*(-y)+(-0.5)*6.5)-(-0.7)*y)+7.7+((y/(-y)+(-0.1)-y))*(-x)-(x+((-7.1)-(-0.7))))
        //printfn "%d" <| 678
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-x)*(-5.4)*(-y)+(-0.5)*6.5)-(-0.7)*y)+7.7+((y/(-y)+(-0.1)-y))*(-x)-(x+((-7.1)-(-0.7))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-p)*(-5.4)*(-q)+(-0.5)*6.5)-(-0.7)*q)+7.7+((q/(-q)+(-0.1)-q))*(-p)-(p+((-7.1)-(-0.7))))
            z2 <== ((((-x)*(-5.4)*(-y)+(-0.5)*6.5)-(-0.7)*y)+7.7+((y/(-y)+(-0.1)-y))*(-x)-(x+((-7.1)-(-0.7))))
            wr.tt <| (I 678)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 679
        ctx.emit.comment "test679"
        //let z0 = (-y)
        //printfn "%d" <| 679
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 679)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 680
        ctx.emit.comment "test680"
        //let z0 = 1.4
        //printfn "%d" <| 680
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 681
        ctx.emit.comment "test681"
        //let z0 = (-x)
        //printfn "%d" <| 681
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 681)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 682
        ctx.emit.comment "test682"
        //let z0 = (y)
        //printfn "%d" <| 682
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q)
            z2 <== (y)
            wr.tt <| (I 682)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 683
        ctx.emit.comment "test683"
        //let z0 = (-2.6)
        //printfn "%d" <| 683
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 684
        ctx.emit.comment "test684"
        //let z0 = 6.7
        //printfn "%d" <| 684
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 685
        ctx.emit.comment "test685"
        //let z0 = 6.3
        //printfn "%d" <| 685
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 686
        ctx.emit.comment "test686"
        //let z0 = 8.6
        //printfn "%d" <| 686
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 687
        ctx.emit.comment "test687"
        //let z0 = 1.4
        //printfn "%d" <| 687
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 688
        ctx.emit.comment "test688"
        //let z0 = ((-0.0)*(-2.5)-(-5.6)-((-2.8)-y-(-y)/y*((-x)-(-7.2)/4.4-(-x))-(y/(-7.4)*(-1.0)/(-y)-(-6.1))+((-x)*8.7/y+4.6-y)))
        //printfn "%d" <| 688
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.0)*(-2.5)-(-5.6)-((-2.8)-y-(-y)/y*((-x)-(-7.2)/4.4-(-x))-(y/(-7.4)*(-1.0)/(-y)-(-6.1))+((-x)*8.7/y+4.6-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.0)*(-2.5)-(-5.6)-((-2.8)-q-(-q)/q*((-p)-(-7.2)/4.4-(-p))-(q/(-7.4)*(-1.0)/(-q)-(-6.1))+((-p)*8.7/q+4.6-q)))
            z2 <== ((-0.0)*(-2.5)-(-5.6)-((-2.8)-y-(-y)/y*((-x)-(-7.2)/4.4-(-x))-(y/(-7.4)*(-1.0)/(-y)-(-6.1))+((-x)*8.7/y+4.6-y)))
            wr.tt <| (I 688)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 689
        ctx.emit.comment "test689"
        //let z0 = 5.6
        //printfn "%d" <| 689
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 690
        ctx.emit.comment "test690"
        //let z0 = (-y)*(((-x))+0.7-6.7/(-y)-(-y)*(-0.0)-2.6-(-x)*(7.5-(-x)))+x
        //printfn "%d" <| 690
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)*(((-x))+0.7-6.7/(-y)-(-y)*(-0.0)-2.6-(-x)*(7.5-(-x)))+x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)*(((-p))+0.7-6.7/(-q)-(-q)*(-0.0)-2.6-(-p)*(7.5-(-p)))+p
            z2 <== (-y)*(((-x))+0.7-6.7/(-y)-(-y)*(-0.0)-2.6-(-x)*(7.5-(-x)))+x
            wr.tt <| (I 690)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 691
        ctx.emit.comment "test691"
        //let z0 = (7.3/(((-1.0)-y-(-2.1)+x-(-y))-(-5.8)/2.8)*(y+(-y)*x)/(((-x)+(-5.5)/(-5.6))*(-x)-(-1.3))*(x/(-y)))
        //printfn "%d" <| 691
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.3/(((-1.0)-y-(-2.1)+x-(-y))-(-5.8)/2.8)*(y+(-y)*x)/(((-x)+(-5.5)/(-5.6))*(-x)-(-1.3))*(x/(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.3/(((-1.0)-q-(-2.1)+p-(-q))-(-5.8)/2.8)*(q+(-q)*p)/(((-p)+(-5.5)/(-5.6))*(-p)-(-1.3))*(p/(-q)))
            z2 <== (7.3/(((-1.0)-y-(-2.1)+x-(-y))-(-5.8)/2.8)*(y+(-y)*x)/(((-x)+(-5.5)/(-5.6))*(-x)-(-1.3))*(x/(-y)))
            wr.tt <| (I 691)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 692
        ctx.emit.comment "test692"
        //let z0 = (x*((8.5+(-x)-x)+(-x)))
        //printfn "%d" <| 692
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x*((8.5+(-x)-x)+(-x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p*((8.5+(-p)-p)+(-p)))
            z2 <== (x*((8.5+(-x)-x)+(-x)))
            wr.tt <| (I 692)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 693
        ctx.emit.comment "test693"
        //let z0 = (((1.8*(-7.0)*(-6.7)+(-1.6))+(1.8*3.7*0.8)-2.2)-(-y))
        //printfn "%d" <| 693
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((1.8*(-7.0)*(-6.7)+(-1.6))+(1.8*3.7*0.8)-2.2)-(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((1.8*(-7.0)*(-6.7)+(-1.6))+(1.8*3.7*0.8)-2.2)-(-q))
            z2 <== (((1.8*(-7.0)*(-6.7)+(-1.6))+(1.8*3.7*0.8)-2.2)-(-y))
            wr.tt <| (I 693)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 694
        ctx.emit.comment "test694"
        //let z0 = 6.7
        //printfn "%d" <| 694
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 695
        ctx.emit.comment "test695"
        //let z0 = ((-7.7)+(x/((-x)-x*(-2.6)*x)*(-4.8)+((-x)*3.7))+5.4*(-x)+x/(-4.0)*(3.8/y+(-7.1)+(-3.8)+(-2.8))-(-2.4))
        //printfn "%d" <| 695
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.7)+(x/((-x)-x*(-2.6)*x)*(-4.8)+((-x)*3.7))+5.4*(-x)+x/(-4.0)*(3.8/y+(-7.1)+(-3.8)+(-2.8))-(-2.4))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.7)+(p/((-p)-p*(-2.6)*p)*(-4.8)+((-p)*3.7))+5.4*(-p)+p/(-4.0)*(3.8/q+(-7.1)+(-3.8)+(-2.8))-(-2.4))
            z2 <== ((-7.7)+(x/((-x)-x*(-2.6)*x)*(-4.8)+((-x)*3.7))+5.4*(-x)+x/(-4.0)*(3.8/y+(-7.1)+(-3.8)+(-2.8))-(-2.4))
            wr.tt <| (I 695)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 696
        ctx.emit.comment "test696"
        //let z0 = (((-y)-(-0.8)*((-x)*(-7.6)/1.3+(-x))-(-8.7)+(x/(-y)-(-1.2)+y)/(-4.6))/(-4.5)-(-y)/5.2)
        //printfn "%d" <| 696
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)-(-0.8)*((-x)*(-7.6)/1.3+(-x))-(-8.7)+(x/(-y)-(-1.2)+y)/(-4.6))/(-4.5)-(-y)/5.2)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)-(-0.8)*((-p)*(-7.6)/1.3+(-p))-(-8.7)+(p/(-q)-(-1.2)+q)/(-4.6))/(-4.5)-(-q)/5.2)
            z2 <== (((-y)-(-0.8)*((-x)*(-7.6)/1.3+(-x))-(-8.7)+(x/(-y)-(-1.2)+y)/(-4.6))/(-4.5)-(-y)/5.2)
            wr.tt <| (I 696)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 697
        ctx.emit.comment "test697"
        //let z0 = (5.6)*5.6+(-x)/1.3
        //printfn "%d" <| 697
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.6)*5.6+(-x)/1.3).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.6)*5.6+(-p)/1.3
            z2 <== (5.6)*5.6+(-x)/1.3
            wr.tt <| (I 697)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 698
        ctx.emit.comment "test698"
        //let z0 = ((y/((-x)+(-0.5)))*6.7/((-3.0)))
        //printfn "%d" <| 698
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y/((-x)+(-0.5)))*6.7/((-3.0)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q/((-p)+(-0.5)))*6.7/((-3.0)))
            z2 <== ((y/((-x)+(-0.5)))*6.7/((-3.0)))
            wr.tt <| (I 698)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 699
        ctx.emit.comment "test699"
        //let z0 = ((x*(-y)-y*((-7.7)-(-4.2))))
        //printfn "%d" <| 699
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x*(-y)-y*((-7.7)-(-4.2))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p*(-q)-q*((-7.7)-(-4.2))))
            z2 <== ((x*(-y)-y*((-7.7)-(-4.2))))
            wr.tt <| (I 699)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 700
        ctx.emit.comment "test700"
        //let z0 = 7.8
        //printfn "%d" <| 700
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 701
        ctx.emit.comment "test701"
        //let z0 = (((x)*(-7.7)-(-4.0))/(8.4/((-x)-x+(-7.3)-x-(-y))+((-7.6)-1.5-(-y))+((-4.0)/8.8/(-5.0)+x-y)+((-4.6)))+((-y)*(-6.2)-(-3.7)-(y-4.4))*(-x))
        //printfn "%d" <| 701
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((x)*(-7.7)-(-4.0))/(8.4/((-x)-x+(-7.3)-x-(-y))+((-7.6)-1.5-(-y))+((-4.0)/8.8/(-5.0)+x-y)+((-4.6)))+((-y)*(-6.2)-(-3.7)-(y-4.4))*(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((p)*(-7.7)-(-4.0))/(8.4/((-p)-p+(-7.3)-p-(-q))+((-7.6)-1.5-(-q))+((-4.0)/8.8/(-5.0)+p-q)+((-4.6)))+((-q)*(-6.2)-(-3.7)-(q-4.4))*(-p))
            z2 <== (((x)*(-7.7)-(-4.0))/(8.4/((-x)-x+(-7.3)-x-(-y))+((-7.6)-1.5-(-y))+((-4.0)/8.8/(-5.0)+x-y)+((-4.6)))+((-y)*(-6.2)-(-3.7)-(y-4.4))*(-x))
            wr.tt <| (I 701)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 702
        ctx.emit.comment "test702"
        //let z0 = (-x)
        //printfn "%d" <| 702
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 702)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 703
        ctx.emit.comment "test703"
        //let z0 = (2.4+5.6-(-4.4)*7.6)
        //printfn "%d" <| 703
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 704
        ctx.emit.comment "test704"
        //let z0 = 0.6
        //printfn "%d" <| 704
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 705
        ctx.emit.comment "test705"
        //let z0 = y
        //printfn "%d" <| 705
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 705)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 706
        ctx.emit.comment "test706"
        //let z0 = x
        //printfn "%d" <| 706
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 706)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 707
        ctx.emit.comment "test707"
        //let z0 = 4.0
        //printfn "%d" <| 707
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 708
        ctx.emit.comment "test708"
        //let z0 = 2.8
        //printfn "%d" <| 708
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 709
        ctx.emit.comment "test709"
        //let z0 = (((-1.4)/y+(-y)*4.1)+(((-2.2)-(-x)*(-8.1)+y)/6.6/(-6.6)/(-3.3)*(-y)/3.7/x+(-y))-(-y)*(y/(-x)*(y)*4.1)/(-y))
        //printfn "%d" <| 709
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-1.4)/y+(-y)*4.1)+(((-2.2)-(-x)*(-8.1)+y)/6.6/(-6.6)/(-3.3)*(-y)/3.7/x+(-y))-(-y)*(y/(-x)*(y)*4.1)/(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-1.4)/q+(-q)*4.1)+(((-2.2)-(-p)*(-8.1)+q)/6.6/(-6.6)/(-3.3)*(-q)/3.7/p+(-q))-(-q)*(q/(-p)*(q)*4.1)/(-q))
            z2 <== (((-1.4)/y+(-y)*4.1)+(((-2.2)-(-x)*(-8.1)+y)/6.6/(-6.6)/(-3.3)*(-y)/3.7/x+(-y))-(-y)*(y/(-x)*(y)*4.1)/(-y))
            wr.tt <| (I 709)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 710
        ctx.emit.comment "test710"
        //let z0 = ((x-(4.2*y*(-x))+(6.1/(-x)+x)*(-x)*(-y)*x+(-y)/(-3.6))*(-y)+((-x)/x*((-y)/3.0/0.6/7.3)*(-8.2)*(-5.2)/y+6.5-x*y)+((-1.4)-6.7))
        //printfn "%d" <| 710
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x-(4.2*y*(-x))+(6.1/(-x)+x)*(-x)*(-y)*x+(-y)/(-3.6))*(-y)+((-x)/x*((-y)/3.0/0.6/7.3)*(-8.2)*(-5.2)/y+6.5-x*y)+((-1.4)-6.7))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p-(4.2*q*(-p))+(6.1/(-p)+p)*(-p)*(-q)*p+(-q)/(-3.6))*(-q)+((-p)/p*((-q)/3.0/0.6/7.3)*(-8.2)*(-5.2)/q+6.5-p*q)+((-1.4)-6.7))
            z2 <== ((x-(4.2*y*(-x))+(6.1/(-x)+x)*(-x)*(-y)*x+(-y)/(-3.6))*(-y)+((-x)/x*((-y)/3.0/0.6/7.3)*(-8.2)*(-5.2)/y+6.5-x*y)+((-1.4)-6.7))
            wr.tt <| (I 710)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 711
        ctx.emit.comment "test711"
        //let z0 = ((-7.7)*(3.2*((-y)*(-y)+6.7)+(-3.6))+(((-y)/(-y))+(-1.2)/(-y))+(-6.3)-(((-1.6)/(-4.7)*(-y)-8.8/(-x))/y*(8.1+(-5.4)/y/(-y)/(-0.6))))
        //printfn "%d" <| 711
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.7)*(3.2*((-y)*(-y)+6.7)+(-3.6))+(((-y)/(-y))+(-1.2)/(-y))+(-6.3)-(((-1.6)/(-4.7)*(-y)-8.8/(-x))/y*(8.1+(-5.4)/y/(-y)/(-0.6))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.7)*(3.2*((-q)*(-q)+6.7)+(-3.6))+(((-q)/(-q))+(-1.2)/(-q))+(-6.3)-(((-1.6)/(-4.7)*(-q)-8.8/(-p))/q*(8.1+(-5.4)/q/(-q)/(-0.6))))
            z2 <== ((-7.7)*(3.2*((-y)*(-y)+6.7)+(-3.6))+(((-y)/(-y))+(-1.2)/(-y))+(-6.3)-(((-1.6)/(-4.7)*(-y)-8.8/(-x))/y*(8.1+(-5.4)/y/(-y)/(-0.6))))
            wr.tt <| (I 711)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 712
        ctx.emit.comment "test712"
        //let z0 = (((x-(-y))*2.4/(y)+((-y)+y)))
        //printfn "%d" <| 712
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((x-(-y))*2.4/(y)+((-y)+y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((p-(-q))*2.4/(q)+((-q)+q)))
            z2 <== (((x-(-y))*2.4/(y)+((-y)+y)))
            wr.tt <| (I 712)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 713
        ctx.emit.comment "test713"
        //let z0 = x
        //printfn "%d" <| 713
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 713)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 714
        ctx.emit.comment "test714"
        //let z0 = x
        //printfn "%d" <| 714
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 714)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 715
        ctx.emit.comment "test715"
        //let z0 = ((-y)*3.0)
        //printfn "%d" <| 715
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)*3.0)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)*3.0)
            z2 <== ((-y)*3.0)
            wr.tt <| (I 715)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 716
        ctx.emit.comment "test716"
        //let z0 = (((-5.5)+x*x*3.8*((-7.2)*(-8.6)*(-x))/(x))-4.3/(((-7.7)/2.8)+7.7/y+(-x))-(x+x+x)/(-x))
        //printfn "%d" <| 716
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-5.5)+x*x*3.8*((-7.2)*(-8.6)*(-x))/(x))-4.3/(((-7.7)/2.8)+7.7/y+(-x))-(x+x+x)/(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-5.5)+p*p*3.8*((-7.2)*(-8.6)*(-p))/(p))-4.3/(((-7.7)/2.8)+7.7/q+(-p))-(p+p+p)/(-p))
            z2 <== (((-5.5)+x*x*3.8*((-7.2)*(-8.6)*(-x))/(x))-4.3/(((-7.7)/2.8)+7.7/y+(-x))-(x+x+x)/(-x))
            wr.tt <| (I 716)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 717
        ctx.emit.comment "test717"
        //let z0 = 5.7
        //printfn "%d" <| 717
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 718
        ctx.emit.comment "test718"
        //let z0 = (6.2*3.8-(y+(-x)+(-1.6))/7.1-(-y)*(-3.1)+((2.8/(-6.5)*x-(-6.7)-(-0.1))))
        //printfn "%d" <| 718
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((6.2*3.8-(y+(-x)+(-1.6))/7.1-(-y)*(-3.1)+((2.8/(-6.5)*x-(-6.7)-(-0.1))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (6.2*3.8-(q+(-p)+(-1.6))/7.1-(-q)*(-3.1)+((2.8/(-6.5)*p-(-6.7)-(-0.1))))
            z2 <== (6.2*3.8-(y+(-x)+(-1.6))/7.1-(-y)*(-3.1)+((2.8/(-6.5)*x-(-6.7)-(-0.1))))
            wr.tt <| (I 718)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 719
        ctx.emit.comment "test719"
        //let z0 = (x*(-1.6)*1.6*(x/x/x+(-2.4)/3.1)-(-2.8)+y*(y+(-y))/((-y)-y-x+(4.1*y*6.5)+(-7.2)/x+(-7.6)*(-2.1)-(-y)))
        //printfn "%d" <| 719
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x*(-1.6)*1.6*(x/x/x+(-2.4)/3.1)-(-2.8)+y*(y+(-y))/((-y)-y-x+(4.1*y*6.5)+(-7.2)/x+(-7.6)*(-2.1)-(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p*(-1.6)*1.6*(p/p/p+(-2.4)/3.1)-(-2.8)+q*(q+(-q))/((-q)-q-p+(4.1*q*6.5)+(-7.2)/p+(-7.6)*(-2.1)-(-q)))
            z2 <== (x*(-1.6)*1.6*(x/x/x+(-2.4)/3.1)-(-2.8)+y*(y+(-y))/((-y)-y-x+(4.1*y*6.5)+(-7.2)/x+(-7.6)*(-2.1)-(-y)))
            wr.tt <| (I 719)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 720
        ctx.emit.comment "test720"
        //let z0 = ((-y)*1.6-8.5/(8.1-(-y)+6.4))+8.5-y*y/y*(y/(6.4+(-x)/0.1)/((-0.4)))
        //printfn "%d" <| 720
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)*1.6-8.5/(8.1-(-y)+6.4))+8.5-y*y/y*(y/(6.4+(-x)/0.1)/((-0.4)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)*1.6-8.5/(8.1-(-q)+6.4))+8.5-q*q/q*(q/(6.4+(-p)/0.1)/((-0.4)))
            z2 <== ((-y)*1.6-8.5/(8.1-(-y)+6.4))+8.5-y*y/y*(y/(6.4+(-x)/0.1)/((-0.4)))
            wr.tt <| (I 720)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 721
        ctx.emit.comment "test721"
        //let z0 = (3.2+((-1.2))-4.2-(-3.2)/x-x/y*y)*(-y)-1.6*(y)-x
        //printfn "%d" <| 721
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((3.2+((-1.2))-4.2-(-3.2)/x-x/y*y)*(-y)-1.6*(y)-x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (3.2+((-1.2))-4.2-(-3.2)/p-p/q*q)*(-q)-1.6*(q)-p
            z2 <== (3.2+((-1.2))-4.2-(-3.2)/x-x/y*y)*(-y)-1.6*(y)-x
            wr.tt <| (I 721)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 722
        ctx.emit.comment "test722"
        //let z0 = (-y)+(((-y))-0.7-x+y/y-(-6.1))
        //printfn "%d" <| 722
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)+(((-y))-0.7-x+y/y-(-6.1))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)+(((-q))-0.7-p+q/q-(-6.1))
            z2 <== (-y)+(((-y))-0.7-x+y/y-(-6.1))
            wr.tt <| (I 722)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 723
        ctx.emit.comment "test723"
        //let z0 = (-2.1)
        //printfn "%d" <| 723
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 724
        ctx.emit.comment "test724"
        //let z0 = ((2.0*(1.7+y+(-2.7))*(-4.4)/x)/(8.2*(-y))+(((-y)/0.2+(-8.4)*(-2.4)+y))/(-4.7)*(-x))
        //printfn "%d" <| 724
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((2.0*(1.7+y+(-2.7))*(-4.4)/x)/(8.2*(-y))+(((-y)/0.2+(-8.4)*(-2.4)+y))/(-4.7)*(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((2.0*(1.7+q+(-2.7))*(-4.4)/p)/(8.2*(-q))+(((-q)/0.2+(-8.4)*(-2.4)+q))/(-4.7)*(-p))
            z2 <== ((2.0*(1.7+y+(-2.7))*(-4.4)/x)/(8.2*(-y))+(((-y)/0.2+(-8.4)*(-2.4)+y))/(-4.7)*(-x))
            wr.tt <| (I 724)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 725
        ctx.emit.comment "test725"
        //let z0 = ((((-x)*4.7/(-y)-(-4.3))+((-0.0)*y+y)+y*2.0)+((-x)-(-x)/(x*6.0-(-3.0))-3.1+(1.6-y/y)*(-6.6))+3.6-4.4-(-y))
        //printfn "%d" <| 725
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-x)*4.7/(-y)-(-4.3))+((-0.0)*y+y)+y*2.0)+((-x)-(-x)/(x*6.0-(-3.0))-3.1+(1.6-y/y)*(-6.6))+3.6-4.4-(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-p)*4.7/(-q)-(-4.3))+((-0.0)*q+q)+q*2.0)+((-p)-(-p)/(p*6.0-(-3.0))-3.1+(1.6-q/q)*(-6.6))+3.6-4.4-(-q))
            z2 <== ((((-x)*4.7/(-y)-(-4.3))+((-0.0)*y+y)+y*2.0)+((-x)-(-x)/(x*6.0-(-3.0))-3.1+(1.6-y/y)*(-6.6))+3.6-4.4-(-y))
            wr.tt <| (I 725)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 726
        ctx.emit.comment "test726"
        //let z0 = y
        //printfn "%d" <| 726
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 726)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 727
        ctx.emit.comment "test727"
        //let z0 = (-y)
        //printfn "%d" <| 727
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 727)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 728
        ctx.emit.comment "test728"
        //let z0 = (((-4.8)+(-2.5)+1.4*((-7.1))/(x/6.7+(-x)))/5.6*(6.6-(-y)-(-0.3)/(-4.0)-(-0.1)))
        //printfn "%d" <| 728
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-4.8)+(-2.5)+1.4*((-7.1))/(x/6.7+(-x)))/5.6*(6.6-(-y)-(-0.3)/(-4.0)-(-0.1)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-4.8)+(-2.5)+1.4*((-7.1))/(p/6.7+(-p)))/5.6*(6.6-(-q)-(-0.3)/(-4.0)-(-0.1)))
            z2 <== (((-4.8)+(-2.5)+1.4*((-7.1))/(x/6.7+(-x)))/5.6*(6.6-(-y)-(-0.3)/(-4.0)-(-0.1)))
            wr.tt <| (I 728)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 729
        ctx.emit.comment "test729"
        //let z0 = (-1.0)
        //printfn "%d" <| 729
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 730
        ctx.emit.comment "test730"
        //let z0 = (-3.6)
        //printfn "%d" <| 730
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 731
        ctx.emit.comment "test731"
        //let z0 = ((-y))
        //printfn "%d" <| 731
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q))
            z2 <== ((-y))
            wr.tt <| (I 731)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 732
        ctx.emit.comment "test732"
        //let z0 = (y+((-1.4)-(-y)+x+(-4.0)*((-1.5)-5.2))+(-y)-x)
        //printfn "%d" <| 732
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+((-1.4)-(-y)+x+(-4.0)*((-1.5)-5.2))+(-y)-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+((-1.4)-(-q)+p+(-4.0)*((-1.5)-5.2))+(-q)-p)
            z2 <== (y+((-1.4)-(-y)+x+(-4.0)*((-1.5)-5.2))+(-y)-x)
            wr.tt <| (I 732)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 733
        ctx.emit.comment "test733"
        //let z0 = (-x)
        //printfn "%d" <| 733
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 733)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 734
        ctx.emit.comment "test734"
        //let z0 = 3.4
        //printfn "%d" <| 734
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 735
        ctx.emit.comment "test735"
        //let z0 = (-7.2)
        //printfn "%d" <| 735
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 736
        ctx.emit.comment "test736"
        //let z0 = (((-y)*(-y)/(8.8)+((-1.6)*(-0.4)+y/y-(-y))*(x+x+(-5.0))))
        //printfn "%d" <| 736
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)*(-y)/(8.8)+((-1.6)*(-0.4)+y/y-(-y))*(x+x+(-5.0))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)*(-q)/(8.8)+((-1.6)*(-0.4)+q/q-(-q))*(p+p+(-5.0))))
            z2 <== (((-y)*(-y)/(8.8)+((-1.6)*(-0.4)+y/y-(-y))*(x+x+(-5.0))))
            wr.tt <| (I 736)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 737
        ctx.emit.comment "test737"
        //let z0 = 6.7
        //printfn "%d" <| 737
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 738
        ctx.emit.comment "test738"
        //let z0 = y
        //printfn "%d" <| 738
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 738)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 739
        ctx.emit.comment "test739"
        //let z0 = (((-y)*y/(y)-y/y)*2.5/(y-1.2-5.1/(-5.6)/x/y+(-5.0))/(3.2))
        //printfn "%d" <| 739
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)*y/(y)-y/y)*2.5/(y-1.2-5.1/(-5.6)/x/y+(-5.0))/(3.2))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)*q/(q)-q/q)*2.5/(q-1.2-5.1/(-5.6)/p/q+(-5.0))/(3.2))
            z2 <== (((-y)*y/(y)-y/y)*2.5/(y-1.2-5.1/(-5.6)/x/y+(-5.0))/(3.2))
            wr.tt <| (I 739)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 740
        ctx.emit.comment "test740"
        //let z0 = ((-y))
        //printfn "%d" <| 740
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q))
            z2 <== ((-y))
            wr.tt <| (I 740)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 741
        ctx.emit.comment "test741"
        //let z0 = 7.4
        //printfn "%d" <| 741
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 742
        ctx.emit.comment "test742"
        //let z0 = (-0.7)/x*(2.4+y+(-2.6)-(-7.8)-y)
        //printfn "%d" <| 742
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-0.7)/x*(2.4+y+(-2.6)-(-7.8)-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-0.7)/p*(2.4+q+(-2.6)-(-7.8)-q)
            z2 <== (-0.7)/x*(2.4+y+(-2.6)-(-7.8)-y)
            wr.tt <| (I 742)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 743
        ctx.emit.comment "test743"
        //let z0 = 7.0
        //printfn "%d" <| 743
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 744
        ctx.emit.comment "test744"
        //let z0 = ((((-6.6)+(-y))*(7.6*(-0.3)+x*(-7.2)*y)*y+(-2.8)+y+4.0-(-3.5))+(2.5)/0.4-(-7.5)-(-y)+(-4.6)*(-3.6)-(0.8-4.4+8.2*y*1.0)*(-y)/((-y)))
        //printfn "%d" <| 744
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-6.6)+(-y))*(7.6*(-0.3)+x*(-7.2)*y)*y+(-2.8)+y+4.0-(-3.5))+(2.5)/0.4-(-7.5)-(-y)+(-4.6)*(-3.6)-(0.8-4.4+8.2*y*1.0)*(-y)/((-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-6.6)+(-q))*(7.6*(-0.3)+p*(-7.2)*q)*q+(-2.8)+q+4.0-(-3.5))+(2.5)/0.4-(-7.5)-(-q)+(-4.6)*(-3.6)-(0.8-4.4+8.2*q*1.0)*(-q)/((-q)))
            z2 <== ((((-6.6)+(-y))*(7.6*(-0.3)+x*(-7.2)*y)*y+(-2.8)+y+4.0-(-3.5))+(2.5)/0.4-(-7.5)-(-y)+(-4.6)*(-3.6)-(0.8-4.4+8.2*y*1.0)*(-y)/((-y)))
            wr.tt <| (I 744)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 745
        ctx.emit.comment "test745"
        //let z0 = (-x)
        //printfn "%d" <| 745
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 745)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 746
        ctx.emit.comment "test746"
        //let z0 = 0.8
        //printfn "%d" <| 746
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 747
        ctx.emit.comment "test747"
        //let z0 = 3.0
        //printfn "%d" <| 747
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 748
        ctx.emit.comment "test748"
        //let z0 = x
        //printfn "%d" <| 748
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 748)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 749
        ctx.emit.comment "test749"
        //let z0 = ((-6.1)-(y)+(x)+((4.8)*(-1.0)/((-y)/(-0.5)*(-y)+8.7-6.1)/y*2.1)-x)
        //printfn "%d" <| 749
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-6.1)-(y)+(x)+((4.8)*(-1.0)/((-y)/(-0.5)*(-y)+8.7-6.1)/y*2.1)-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-6.1)-(q)+(p)+((4.8)*(-1.0)/((-q)/(-0.5)*(-q)+8.7-6.1)/q*2.1)-p)
            z2 <== ((-6.1)-(y)+(x)+((4.8)*(-1.0)/((-y)/(-0.5)*(-y)+8.7-6.1)/y*2.1)-x)
            wr.tt <| (I 749)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 750
        ctx.emit.comment "test750"
        //let z0 = (-y)
        //printfn "%d" <| 750
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 750)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 751
        ctx.emit.comment "test751"
        //let z0 = ((y*(-2.2)+(-4.5)-(-3.8))*(-x))
        //printfn "%d" <| 751
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y*(-2.2)+(-4.5)-(-3.8))*(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q*(-2.2)+(-4.5)-(-3.8))*(-p))
            z2 <== ((y*(-2.2)+(-4.5)-(-3.8))*(-x))
            wr.tt <| (I 751)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 752
        ctx.emit.comment "test752"
        //let z0 = (-4.3)
        //printfn "%d" <| 752
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 753
        ctx.emit.comment "test753"
        //let z0 = (-x)
        //printfn "%d" <| 753
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 753)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 754
        ctx.emit.comment "test754"
        //let z0 = ((((-1.6)*6.6*1.8+(-6.2)/(-4.7))+((-y)-(-8.7)+(-1.1)+(-x))/(-y)-8.0)*((-7.7)*8.4/(-3.8)+(-3.6))-(2.6/x*(-x)*(-y)))
        //printfn "%d" <| 754
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-1.6)*6.6*1.8+(-6.2)/(-4.7))+((-y)-(-8.7)+(-1.1)+(-x))/(-y)-8.0)*((-7.7)*8.4/(-3.8)+(-3.6))-(2.6/x*(-x)*(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-1.6)*6.6*1.8+(-6.2)/(-4.7))+((-q)-(-8.7)+(-1.1)+(-p))/(-q)-8.0)*((-7.7)*8.4/(-3.8)+(-3.6))-(2.6/p*(-p)*(-q)))
            z2 <== ((((-1.6)*6.6*1.8+(-6.2)/(-4.7))+((-y)-(-8.7)+(-1.1)+(-x))/(-y)-8.0)*((-7.7)*8.4/(-3.8)+(-3.6))-(2.6/x*(-x)*(-y)))
            wr.tt <| (I 754)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 755
        ctx.emit.comment "test755"
        //let z0 = (-8.4)
        //printfn "%d" <| 755
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 756
        ctx.emit.comment "test756"
        //let z0 = (x-(((-x)/x/(-x)+x-(-3.8))-4.8/y/((-3.6)*3.5)-5.6)-(-8.7)-(-5.2)/((-y)/(-y)/8.7*(-y)-y)/x)
        //printfn "%d" <| 756
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x-(((-x)/x/(-x)+x-(-3.8))-4.8/y/((-3.6)*3.5)-5.6)-(-8.7)-(-5.2)/((-y)/(-y)/8.7*(-y)-y)/x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p-(((-p)/p/(-p)+p-(-3.8))-4.8/q/((-3.6)*3.5)-5.6)-(-8.7)-(-5.2)/((-q)/(-q)/8.7*(-q)-q)/p)
            z2 <== (x-(((-x)/x/(-x)+x-(-3.8))-4.8/y/((-3.6)*3.5)-5.6)-(-8.7)-(-5.2)/((-y)/(-y)/8.7*(-y)-y)/x)
            wr.tt <| (I 756)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 757
        ctx.emit.comment "test757"
        //let z0 = 8.7
        //printfn "%d" <| 757
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 758
        ctx.emit.comment "test758"
        //let z0 = y
        //printfn "%d" <| 758
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 758)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 759
        ctx.emit.comment "test759"
        //let z0 = (((-8.7)-x-(y/(-y))+((-1.1)/x*y-(-x)*3.1)))
        //printfn "%d" <| 759
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-8.7)-x-(y/(-y))+((-1.1)/x*y-(-x)*3.1)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-8.7)-p-(q/(-q))+((-1.1)/p*q-(-p)*3.1)))
            z2 <== (((-8.7)-x-(y/(-y))+((-1.1)/x*y-(-x)*3.1)))
            wr.tt <| (I 759)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 760
        ctx.emit.comment "test760"
        //let z0 = (3.0/3.1*(2.5))
        //printfn "%d" <| 760
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 761
        ctx.emit.comment "test761"
        //let z0 = (y/(-y)+x+(-5.6)*1.3+y*(-y))
        //printfn "%d" <| 761
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y/(-y)+x+(-5.6)*1.3+y*(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q/(-q)+p+(-5.6)*1.3+q*(-q))
            z2 <== (y/(-y)+x+(-5.6)*1.3+y*(-y))
            wr.tt <| (I 761)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 762
        ctx.emit.comment "test762"
        //let z0 = y
        //printfn "%d" <| 762
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 762)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 763
        ctx.emit.comment "test763"
        //let z0 = (6.6/1.5-(y-(6.8))*(8.6-x*0.1))
        //printfn "%d" <| 763
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((6.6/1.5-(y-(6.8))*(8.6-x*0.1))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (6.6/1.5-(q-(6.8))*(8.6-p*0.1))
            z2 <== (6.6/1.5-(y-(6.8))*(8.6-x*0.1))
            wr.tt <| (I 763)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 764
        ctx.emit.comment "test764"
        //let z0 = x
        //printfn "%d" <| 764
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 764)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 765
        ctx.emit.comment "test765"
        //let z0 = x
        //printfn "%d" <| 765
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 765)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 766
        ctx.emit.comment "test766"
        //let z0 = (y-(-2.8))
        //printfn "%d" <| 766
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y-(-2.8))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q-(-2.8))
            z2 <== (y-(-2.8))
            wr.tt <| (I 766)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 767
        ctx.emit.comment "test767"
        //let z0 = (4.2)
        //printfn "%d" <| 767
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 768
        ctx.emit.comment "test768"
        //let z0 = 7.4
        //printfn "%d" <| 768
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 769
        ctx.emit.comment "test769"
        //let z0 = (0.0+((-x)/3.8/((-x)-x+(-4.6)-y))*(2.7+(-8.6)-y)*(y+(-3.2)-(-y)/(-7.2)+1.4)-x+(-2.6))
        //printfn "%d" <| 769
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((0.0+((-x)/3.8/((-x)-x+(-4.6)-y))*(2.7+(-8.6)-y)*(y+(-3.2)-(-y)/(-7.2)+1.4)-x+(-2.6))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (0.0+((-p)/3.8/((-p)-p+(-4.6)-q))*(2.7+(-8.6)-q)*(q+(-3.2)-(-q)/(-7.2)+1.4)-p+(-2.6))
            z2 <== (0.0+((-x)/3.8/((-x)-x+(-4.6)-y))*(2.7+(-8.6)-y)*(y+(-3.2)-(-y)/(-7.2)+1.4)-x+(-2.6))
            wr.tt <| (I 769)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 770
        ctx.emit.comment "test770"
        //let z0 = ((-y)/(-x)*(5.4+y-4.8*x)*(-y)*(-y)-((-x)*(-x)+(y/3.0)+2.7-y)/(((-y))/y))
        //printfn "%d" <| 770
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)/(-x)*(5.4+y-4.8*x)*(-y)*(-y)-((-x)*(-x)+(y/3.0)+2.7-y)/(((-y))/y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)/(-p)*(5.4+q-4.8*p)*(-q)*(-q)-((-p)*(-p)+(q/3.0)+2.7-q)/(((-q))/q))
            z2 <== ((-y)/(-x)*(5.4+y-4.8*x)*(-y)*(-y)-((-x)*(-x)+(y/3.0)+2.7-y)/(((-y))/y))
            wr.tt <| (I 770)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 771
        ctx.emit.comment "test771"
        //let z0 = ((-7.4)*2.3/(-y)*y+x)
        //printfn "%d" <| 771
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.4)*2.3/(-y)*y+x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.4)*2.3/(-q)*q+p)
            z2 <== ((-7.4)*2.3/(-y)*y+x)
            wr.tt <| (I 771)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 772
        ctx.emit.comment "test772"
        //let z0 = ((-y)-x-((-y)+5.0*x))
        //printfn "%d" <| 772
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)-x-((-y)+5.0*x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)-p-((-q)+5.0*p))
            z2 <== ((-y)-x-((-y)+5.0*x))
            wr.tt <| (I 772)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 773
        ctx.emit.comment "test773"
        //let z0 = (-x)-(2.5*(y-x*(-y)/y+(-y))*((-0.1)/y))+(-x)*(-4.1)+y
        //printfn "%d" <| 773
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)-(2.5*(y-x*(-y)/y+(-y))*((-0.1)/y))+(-x)*(-4.1)+y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)-(2.5*(q-p*(-q)/q+(-q))*((-0.1)/q))+(-p)*(-4.1)+q
            z2 <== (-x)-(2.5*(y-x*(-y)/y+(-y))*((-0.1)/y))+(-x)*(-4.1)+y
            wr.tt <| (I 773)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 774
        ctx.emit.comment "test774"
        //let z0 = ((((-2.5)*3.5/(-x)+(-1.3))-(y-(-6.3)-7.0/y))/((y))+1.8/y)
        //printfn "%d" <| 774
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-2.5)*3.5/(-x)+(-1.3))-(y-(-6.3)-7.0/y))/((y))+1.8/y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-2.5)*3.5/(-p)+(-1.3))-(q-(-6.3)-7.0/q))/((q))+1.8/q)
            z2 <== ((((-2.5)*3.5/(-x)+(-1.3))-(y-(-6.3)-7.0/y))/((y))+1.8/y)
            wr.tt <| (I 774)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 775
        ctx.emit.comment "test775"
        //let z0 = 3.8
        //printfn "%d" <| 775
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 776
        ctx.emit.comment "test776"
        //let z0 = (-y)
        //printfn "%d" <| 776
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 776)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 777
        ctx.emit.comment "test777"
        //let z0 = 3.5
        //printfn "%d" <| 777
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 778
        ctx.emit.comment "test778"
        //let z0 = (x)
        //printfn "%d" <| 778
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p)
            z2 <== (x)
            wr.tt <| (I 778)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 779
        ctx.emit.comment "test779"
        //let z0 = (((-y))*((-y)+y+y*(-y))-(5.7/((-7.0)+(-y)+x-(-x))+y+(y+1.4/3.4/y))*((-y)*4.8+7.0/y)*(-y)-(-y)*(-4.1)*(-1.2)+((-x)-x*x-x)+(-8.1)-(3.4*(-x))+((-6.7)-(-y)))
        //printfn "%d" <| 779
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y))*((-y)+y+y*(-y))-(5.7/((-7.0)+(-y)+x-(-x))+y+(y+1.4/3.4/y))*((-y)*4.8+7.0/y)*(-y)-(-y)*(-4.1)*(-1.2)+((-x)-x*x-x)+(-8.1)-(3.4*(-x))+((-6.7)-(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q))*((-q)+q+q*(-q))-(5.7/((-7.0)+(-q)+p-(-p))+q+(q+1.4/3.4/q))*((-q)*4.8+7.0/q)*(-q)-(-q)*(-4.1)*(-1.2)+((-p)-p*p-p)+(-8.1)-(3.4*(-p))+((-6.7)-(-q)))
            z2 <== (((-y))*((-y)+y+y*(-y))-(5.7/((-7.0)+(-y)+x-(-x))+y+(y+1.4/3.4/y))*((-y)*4.8+7.0/y)*(-y)-(-y)*(-4.1)*(-1.2)+((-x)-x*x-x)+(-8.1)-(3.4*(-x))+((-6.7)-(-y)))
            wr.tt <| (I 779)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 780
        ctx.emit.comment "test780"
        //let z0 = (-y)
        //printfn "%d" <| 780
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 780)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 781
        ctx.emit.comment "test781"
        //let z0 = 3.4
        //printfn "%d" <| 781
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 782
        ctx.emit.comment "test782"
        //let z0 = (x/y+((x-(-y)-(-5.0)+3.5)*(-y)))
        //printfn "%d" <| 782
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x/y+((x-(-y)-(-5.0)+3.5)*(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p/q+((p-(-q)-(-5.0)+3.5)*(-q)))
            z2 <== (x/y+((x-(-y)-(-5.0)+3.5)*(-y)))
            wr.tt <| (I 782)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 783
        ctx.emit.comment "test783"
        //let z0 = y-x*7.3+(3.1/(-3.7))/(-x)/(-y)/x-(-y)*(-4.1)/(y)*((-y)*7.2)/(x)
        //printfn "%d" <| 783
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y-x*7.3+(3.1/(-3.7))/(-x)/(-y)/x-(-y)*(-4.1)/(y)*((-y)*7.2)/(x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q-p*7.3+(3.1/(-3.7))/(-p)/(-q)/p-(-q)*(-4.1)/(q)*((-q)*7.2)/(p)
            z2 <== y-x*7.3+(3.1/(-3.7))/(-x)/(-y)/x-(-y)*(-4.1)/(y)*((-y)*7.2)/(x)
            wr.tt <| (I 783)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 784
        ctx.emit.comment "test784"
        //let z0 = (-y)
        //printfn "%d" <| 784
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 784)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 785
        ctx.emit.comment "test785"
        //let z0 = (-x)
        //printfn "%d" <| 785
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 785)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 786
        ctx.emit.comment "test786"
        //let z0 = (-x)
        //printfn "%d" <| 786
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 786)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 787
        ctx.emit.comment "test787"
        //let z0 = (-y)
        //printfn "%d" <| 787
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 787)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 788
        ctx.emit.comment "test788"
        //let z0 = ((-3.4)*(-x)-(-x)+((-y)/(-3.6)-6.0*(-x)-(-3.2)/(-1.2))*((y-(-y)/2.8-(-y))*((-x)*x/(-7.6)/(-1.7)-(-8.7))*x))
        //printfn "%d" <| 788
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-3.4)*(-x)-(-x)+((-y)/(-3.6)-6.0*(-x)-(-3.2)/(-1.2))*((y-(-y)/2.8-(-y))*((-x)*x/(-7.6)/(-1.7)-(-8.7))*x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-3.4)*(-p)-(-p)+((-q)/(-3.6)-6.0*(-p)-(-3.2)/(-1.2))*((q-(-q)/2.8-(-q))*((-p)*p/(-7.6)/(-1.7)-(-8.7))*p))
            z2 <== ((-3.4)*(-x)-(-x)+((-y)/(-3.6)-6.0*(-x)-(-3.2)/(-1.2))*((y-(-y)/2.8-(-y))*((-x)*x/(-7.6)/(-1.7)-(-8.7))*x))
            wr.tt <| (I 788)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 789
        ctx.emit.comment "test789"
        //let z0 = ((x-y+6.1-(x)+(-x))/(-6.6)/(4.3-x)-(-1.2)-(7.3)/(-x)-0.7)
        //printfn "%d" <| 789
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x-y+6.1-(x)+(-x))/(-6.6)/(4.3-x)-(-1.2)-(7.3)/(-x)-0.7)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p-q+6.1-(p)+(-p))/(-6.6)/(4.3-p)-(-1.2)-(7.3)/(-p)-0.7)
            z2 <== ((x-y+6.1-(x)+(-x))/(-6.6)/(4.3-x)-(-1.2)-(7.3)/(-x)-0.7)
            wr.tt <| (I 789)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 790
        ctx.emit.comment "test790"
        //let z0 = (6.6/(-y)+3.8-(-8.5)/6.2)+x
        //printfn "%d" <| 790
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((6.6/(-y)+3.8-(-8.5)/6.2)+x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (6.6/(-q)+3.8-(-8.5)/6.2)+p
            z2 <== (6.6/(-y)+3.8-(-8.5)/6.2)+x
            wr.tt <| (I 790)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 791
        ctx.emit.comment "test791"
        //let z0 = (-1.5)
        //printfn "%d" <| 791
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 792
        ctx.emit.comment "test792"
        //let z0 = (((x*x-(-1.6))))
        //printfn "%d" <| 792
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((x*x-(-1.6))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((p*p-(-1.6))))
            z2 <== (((x*x-(-1.6))))
            wr.tt <| (I 792)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 793
        ctx.emit.comment "test793"
        //let z0 = x
        //printfn "%d" <| 793
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 793)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 794
        ctx.emit.comment "test794"
        //let z0 = (((8.7-1.4-(-x)*(-6.1)-(-y))-1.2+(-y))+(7.2-(-x)/(-x))+((y+1.5)-(y+y))*((6.3*(-x)/(-6.8))-7.6*((-y)-(-y))-((-4.4)*(-1.7)-(-y)*(-4.6)))-y)
        //printfn "%d" <| 794
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((8.7-1.4-(-x)*(-6.1)-(-y))-1.2+(-y))+(7.2-(-x)/(-x))+((y+1.5)-(y+y))*((6.3*(-x)/(-6.8))-7.6*((-y)-(-y))-((-4.4)*(-1.7)-(-y)*(-4.6)))-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((8.7-1.4-(-p)*(-6.1)-(-q))-1.2+(-q))+(7.2-(-p)/(-p))+((q+1.5)-(q+q))*((6.3*(-p)/(-6.8))-7.6*((-q)-(-q))-((-4.4)*(-1.7)-(-q)*(-4.6)))-q)
            z2 <== (((8.7-1.4-(-x)*(-6.1)-(-y))-1.2+(-y))+(7.2-(-x)/(-x))+((y+1.5)-(y+y))*((6.3*(-x)/(-6.8))-7.6*((-y)-(-y))-((-4.4)*(-1.7)-(-y)*(-4.6)))-y)
            wr.tt <| (I 794)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 795
        ctx.emit.comment "test795"
        //let z0 = ((y+4.4-x*0.1*(-7.3)-(-3.8)*((-y)*0.2+(-7.1))+2.8)-((8.4+x)+(0.4)/(y-(-x)/5.0+1.6-(-1.1))+(2.2-(-4.0)))+(x+y/5.7+(1.6+(-x))/((-y)*x)))
        //printfn "%d" <| 795
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y+4.4-x*0.1*(-7.3)-(-3.8)*((-y)*0.2+(-7.1))+2.8)-((8.4+x)+(0.4)/(y-(-x)/5.0+1.6-(-1.1))+(2.2-(-4.0)))+(x+y/5.7+(1.6+(-x))/((-y)*x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q+4.4-p*0.1*(-7.3)-(-3.8)*((-q)*0.2+(-7.1))+2.8)-((8.4+p)+(0.4)/(q-(-p)/5.0+1.6-(-1.1))+(2.2-(-4.0)))+(p+q/5.7+(1.6+(-p))/((-q)*p)))
            z2 <== ((y+4.4-x*0.1*(-7.3)-(-3.8)*((-y)*0.2+(-7.1))+2.8)-((8.4+x)+(0.4)/(y-(-x)/5.0+1.6-(-1.1))+(2.2-(-4.0)))+(x+y/5.7+(1.6+(-x))/((-y)*x)))
            wr.tt <| (I 795)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 796
        ctx.emit.comment "test796"
        //let z0 = 8.8+(((-3.3)+y)-3.1/x/(-y))
        //printfn "%d" <| 796
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (8.8+(((-3.3)+y)-3.1/x/(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 8.8+(((-3.3)+q)-3.1/p/(-q))
            z2 <== 8.8+(((-3.3)+y)-3.1/x/(-y))
            wr.tt <| (I 796)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 797
        ctx.emit.comment "test797"
        //let z0 = ((-x)+x/x-((-x)-(-x)*(-3.6))*8.0-(((-2.6)*(-y)*y)+((-y)/x)/1.8-8.3+(-8.6)/y/(-y)+((-y)*6.4-y*y+x))/((-2.1)/1.0*5.1-(-x)+7.7*x/((-y)))/((-x)*(-x)/(-y)))
        //printfn "%d" <| 797
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+x/x-((-x)-(-x)*(-3.6))*8.0-(((-2.6)*(-y)*y)+((-y)/x)/1.8-8.3+(-8.6)/y/(-y)+((-y)*6.4-y*y+x))/((-2.1)/1.0*5.1-(-x)+7.7*x/((-y)))/((-x)*(-x)/(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+p/p-((-p)-(-p)*(-3.6))*8.0-(((-2.6)*(-q)*q)+((-q)/p)/1.8-8.3+(-8.6)/q/(-q)+((-q)*6.4-q*q+p))/((-2.1)/1.0*5.1-(-p)+7.7*p/((-q)))/((-p)*(-p)/(-q)))
            z2 <== ((-x)+x/x-((-x)-(-x)*(-3.6))*8.0-(((-2.6)*(-y)*y)+((-y)/x)/1.8-8.3+(-8.6)/y/(-y)+((-y)*6.4-y*y+x))/((-2.1)/1.0*5.1-(-x)+7.7*x/((-y)))/((-x)*(-x)/(-y)))
            wr.tt <| (I 797)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 798
        ctx.emit.comment "test798"
        //let z0 = (-y)
        //printfn "%d" <| 798
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 798)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 799
        ctx.emit.comment "test799"
        //let z0 = (y+(-y)/(-2.2))
        //printfn "%d" <| 799
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+(-y)/(-2.2))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+(-q)/(-2.2))
            z2 <== (y+(-y)/(-2.2))
            wr.tt <| (I 799)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 800
        ctx.emit.comment "test800"
        //let z0 = 8.7
        //printfn "%d" <| 800
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 801
        ctx.emit.comment "test801"
        //let z0 = 3.6
        //printfn "%d" <| 801
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 802
        ctx.emit.comment "test802"
        //let z0 = (-7.6)-(-6.5)-(-x)*0.6*(-y)/(-x)+5.4+(1.6-(-7.5))*((-8.2)-6.6-1.4/(-x)/(y+(-5.3)/(-7.5)+(-2.0)-(-x))*(-4.1))
        //printfn "%d" <| 802
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-7.6)-(-6.5)-(-x)*0.6*(-y)/(-x)+5.4+(1.6-(-7.5))*((-8.2)-6.6-1.4/(-x)/(y+(-5.3)/(-7.5)+(-2.0)-(-x))*(-4.1))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-7.6)-(-6.5)-(-p)*0.6*(-q)/(-p)+5.4+(1.6-(-7.5))*((-8.2)-6.6-1.4/(-p)/(q+(-5.3)/(-7.5)+(-2.0)-(-p))*(-4.1))
            z2 <== (-7.6)-(-6.5)-(-x)*0.6*(-y)/(-x)+5.4+(1.6-(-7.5))*((-8.2)-6.6-1.4/(-x)/(y+(-5.3)/(-7.5)+(-2.0)-(-x))*(-4.1))
            wr.tt <| (I 802)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 803
        ctx.emit.comment "test803"
        //let z0 = ((8.4+(-x))/(y-(-1.2)*y+5.7)-(3.4*5.2)*5.6+5.3)*((-8.1))/x*(-y)
        //printfn "%d" <| 803
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((8.4+(-x))/(y-(-1.2)*y+5.7)-(3.4*5.2)*5.6+5.3)*((-8.1))/x*(-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((8.4+(-p))/(q-(-1.2)*q+5.7)-(3.4*5.2)*5.6+5.3)*((-8.1))/p*(-q)
            z2 <== ((8.4+(-x))/(y-(-1.2)*y+5.7)-(3.4*5.2)*5.6+5.3)*((-8.1))/x*(-y)
            wr.tt <| (I 803)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 804
        ctx.emit.comment "test804"
        //let z0 = ((-0.4)/(((-x)/(-y)*5.8)*(-7.7)-1.5/(-x)*(-x)-y)-8.7)
        //printfn "%d" <| 804
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.4)/(((-x)/(-y)*5.8)*(-7.7)-1.5/(-x)*(-x)-y)-8.7)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.4)/(((-p)/(-q)*5.8)*(-7.7)-1.5/(-p)*(-p)-q)-8.7)
            z2 <== ((-0.4)/(((-x)/(-y)*5.8)*(-7.7)-1.5/(-x)*(-x)-y)-8.7)
            wr.tt <| (I 804)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 805
        ctx.emit.comment "test805"
        //let z0 = (((-y)+(-x)-(-3.2)*4.6)+(x/(-8.1))+y+y+x)/(-x)-(7.6/(-x)-6.4*(-x)-y)*(-x)*(((-6.4)*1.4-y*(-0.7))+((-x)))+(-3.6)*(y)
        //printfn "%d" <| 805
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)+(-x)-(-3.2)*4.6)+(x/(-8.1))+y+y+x)/(-x)-(7.6/(-x)-6.4*(-x)-y)*(-x)*(((-6.4)*1.4-y*(-0.7))+((-x)))+(-3.6)*(y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)+(-p)-(-3.2)*4.6)+(p/(-8.1))+q+q+p)/(-p)-(7.6/(-p)-6.4*(-p)-q)*(-p)*(((-6.4)*1.4-q*(-0.7))+((-p)))+(-3.6)*(q)
            z2 <== (((-y)+(-x)-(-3.2)*4.6)+(x/(-8.1))+y+y+x)/(-x)-(7.6/(-x)-6.4*(-x)-y)*(-x)*(((-6.4)*1.4-y*(-0.7))+((-x)))+(-3.6)*(y)
            wr.tt <| (I 805)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 806
        ctx.emit.comment "test806"
        //let z0 = (-y)
        //printfn "%d" <| 806
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 806)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 807
        ctx.emit.comment "test807"
        //let z0 = (y/2.2-(-y)-(-6.5)/x/(-8.4)-5.8-5.6/(-y)*((-x)+1.1)/(y-4.0/0.5+(-x)))
        //printfn "%d" <| 807
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y/2.2-(-y)-(-6.5)/x/(-8.4)-5.8-5.6/(-y)*((-x)+1.1)/(y-4.0/0.5+(-x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q/2.2-(-q)-(-6.5)/p/(-8.4)-5.8-5.6/(-q)*((-p)+1.1)/(q-4.0/0.5+(-p)))
            z2 <== (y/2.2-(-y)-(-6.5)/x/(-8.4)-5.8-5.6/(-y)*((-x)+1.1)/(y-4.0/0.5+(-x)))
            wr.tt <| (I 807)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 808
        ctx.emit.comment "test808"
        //let z0 = y
        //printfn "%d" <| 808
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 808)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 809
        ctx.emit.comment "test809"
        //let z0 = (((-y))/((-4.2)*7.0-(-x)-((-y)-(-y))*x))
        //printfn "%d" <| 809
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y))/((-4.2)*7.0-(-x)-((-y)-(-y))*x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q))/((-4.2)*7.0-(-p)-((-q)-(-q))*p))
            z2 <== (((-y))/((-4.2)*7.0-(-x)-((-y)-(-y))*x))
            wr.tt <| (I 809)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 810
        ctx.emit.comment "test810"
        //let z0 = (-7.4)
        //printfn "%d" <| 810
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 811
        ctx.emit.comment "test811"
        //let z0 = (y)
        //printfn "%d" <| 811
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q)
            z2 <== (y)
            wr.tt <| (I 811)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 812
        ctx.emit.comment "test812"
        //let z0 = (4.5/4.3*(-y)+(-1.5)+((x+0.4-1.1)*(-8.6)))
        //printfn "%d" <| 812
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((4.5/4.3*(-y)+(-1.5)+((x+0.4-1.1)*(-8.6)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (4.5/4.3*(-q)+(-1.5)+((p+0.4-1.1)*(-8.6)))
            z2 <== (4.5/4.3*(-y)+(-1.5)+((x+0.4-1.1)*(-8.6)))
            wr.tt <| (I 812)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 813
        ctx.emit.comment "test813"
        //let z0 = y
        //printfn "%d" <| 813
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 813)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 814
        ctx.emit.comment "test814"
        //let z0 = ((-8.8))
        //printfn "%d" <| 814
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 815
        ctx.emit.comment "test815"
        //let z0 = ((-y))
        //printfn "%d" <| 815
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q))
            z2 <== ((-y))
            wr.tt <| (I 815)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 816
        ctx.emit.comment "test816"
        //let z0 = x
        //printfn "%d" <| 816
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 816)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 817
        ctx.emit.comment "test817"
        //let z0 = ((-2.3)/(-x)+(-8.4))
        //printfn "%d" <| 817
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-2.3)/(-x)+(-8.4))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-2.3)/(-p)+(-8.4))
            z2 <== ((-2.3)/(-x)+(-8.4))
            wr.tt <| (I 817)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 818
        ctx.emit.comment "test818"
        //let z0 = (-x)
        //printfn "%d" <| 818
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 818)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 819
        ctx.emit.comment "test819"
        //let z0 = (-y)
        //printfn "%d" <| 819
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 819)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 820
        ctx.emit.comment "test820"
        //let z0 = (y*(-7.6)-3.1)
        //printfn "%d" <| 820
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y*(-7.6)-3.1)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q*(-7.6)-3.1)
            z2 <== (y*(-7.6)-3.1)
            wr.tt <| (I 820)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 821
        ctx.emit.comment "test821"
        //let z0 = (8.5)
        //printfn "%d" <| 821
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 822
        ctx.emit.comment "test822"
        //let z0 = (2.4)+(-2.2)-8.7+x
        //printfn "%d" <| 822
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((2.4)+(-2.2)-8.7+x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (2.4)+(-2.2)-8.7+p
            z2 <== (2.4)+(-2.2)-8.7+x
            wr.tt <| (I 822)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 823
        ctx.emit.comment "test823"
        //let z0 = ((2.7)+(-1.5))
        //printfn "%d" <| 823
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 824
        ctx.emit.comment "test824"
        //let z0 = (((2.5-y*6.6)+0.0*((-x)-3.3-y*(-y))))
        //printfn "%d" <| 824
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((2.5-y*6.6)+0.0*((-x)-3.3-y*(-y))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((2.5-q*6.6)+0.0*((-p)-3.3-q*(-q))))
            z2 <== (((2.5-y*6.6)+0.0*((-x)-3.3-y*(-y))))
            wr.tt <| (I 824)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 825
        ctx.emit.comment "test825"
        //let z0 = (y*(7.1)*(3.2*(6.4)-(-x)+(-y))-(x))
        //printfn "%d" <| 825
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y*(7.1)*(3.2*(6.4)-(-x)+(-y))-(x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q*(7.1)*(3.2*(6.4)-(-p)+(-q))-(p))
            z2 <== (y*(7.1)*(3.2*(6.4)-(-x)+(-y))-(x))
            wr.tt <| (I 825)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 826
        ctx.emit.comment "test826"
        //let z0 = 7.2
        //printfn "%d" <| 826
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 827
        ctx.emit.comment "test827"
        //let z0 = (x*(3.0)-(y-(-x)*(-1.6))+(-x)/(-4.0)/y/(-x)-1.4-(-y)/y)/(-1.6)-(((-x)/2.6*1.4)+((-x))+(8.2*(-x)-y*7.4-6.8)+6.4)/(((-x))-((-x)-0.3/1.8*x)/(x-(-y)/y+(-x)))/6.7
        //printfn "%d" <| 827
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x*(3.0)-(y-(-x)*(-1.6))+(-x)/(-4.0)/y/(-x)-1.4-(-y)/y)/(-1.6)-(((-x)/2.6*1.4)+((-x))+(8.2*(-x)-y*7.4-6.8)+6.4)/(((-x))-((-x)-0.3/1.8*x)/(x-(-y)/y+(-x)))/6.7).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p*(3.0)-(q-(-p)*(-1.6))+(-p)/(-4.0)/q/(-p)-1.4-(-q)/q)/(-1.6)-(((-p)/2.6*1.4)+((-p))+(8.2*(-p)-q*7.4-6.8)+6.4)/(((-p))-((-p)-0.3/1.8*p)/(p-(-q)/q+(-p)))/6.7
            z2 <== (x*(3.0)-(y-(-x)*(-1.6))+(-x)/(-4.0)/y/(-x)-1.4-(-y)/y)/(-1.6)-(((-x)/2.6*1.4)+((-x))+(8.2*(-x)-y*7.4-6.8)+6.4)/(((-x))-((-x)-0.3/1.8*x)/(x-(-y)/y+(-x)))/6.7
            wr.tt <| (I 827)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 828
        ctx.emit.comment "test828"
        //let z0 = (-5.5)
        //printfn "%d" <| 828
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 829
        ctx.emit.comment "test829"
        //let z0 = (-4.5)
        //printfn "%d" <| 829
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 830
        ctx.emit.comment "test830"
        //let z0 = (-x)
        //printfn "%d" <| 830
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 830)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 831
        ctx.emit.comment "test831"
        //let z0 = (((y/(-3.3))*(-6.7)-(8.6*y+(-6.1)*y/(-y))-x/(-7.5))-(-7.7)/y+(x/(-1.6)*(-7.3)/(-y)*(-x)))
        //printfn "%d" <| 831
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((y/(-3.3))*(-6.7)-(8.6*y+(-6.1)*y/(-y))-x/(-7.5))-(-7.7)/y+(x/(-1.6)*(-7.3)/(-y)*(-x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((q/(-3.3))*(-6.7)-(8.6*q+(-6.1)*q/(-q))-p/(-7.5))-(-7.7)/q+(p/(-1.6)*(-7.3)/(-q)*(-p)))
            z2 <== (((y/(-3.3))*(-6.7)-(8.6*y+(-6.1)*y/(-y))-x/(-7.5))-(-7.7)/y+(x/(-1.6)*(-7.3)/(-y)*(-x)))
            wr.tt <| (I 831)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 832
        ctx.emit.comment "test832"
        //let z0 = y
        //printfn "%d" <| 832
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 832)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 833
        ctx.emit.comment "test833"
        //let z0 = ((-5.4)-8.4/0.3/((-0.1)/0.1))
        //printfn "%d" <| 833
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 834
        ctx.emit.comment "test834"
        //let z0 = (-3.8)
        //printfn "%d" <| 834
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 835
        ctx.emit.comment "test835"
        //let z0 = (-5.1)+(((-y)+(-3.4))+(-1.5)*((-y)+(-5.7)/x)*(-8.6))
        //printfn "%d" <| 835
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-5.1)+(((-y)+(-3.4))+(-1.5)*((-y)+(-5.7)/x)*(-8.6))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-5.1)+(((-q)+(-3.4))+(-1.5)*((-q)+(-5.7)/p)*(-8.6))
            z2 <== (-5.1)+(((-y)+(-3.4))+(-1.5)*((-y)+(-5.7)/x)*(-8.6))
            wr.tt <| (I 835)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 836
        ctx.emit.comment "test836"
        //let z0 = (y-(-8.3)/4.8*(-y)-(-y))
        //printfn "%d" <| 836
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y-(-8.3)/4.8*(-y)-(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q-(-8.3)/4.8*(-q)-(-q))
            z2 <== (y-(-8.3)/4.8*(-y)-(-y))
            wr.tt <| (I 836)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 837
        ctx.emit.comment "test837"
        //let z0 = ((-6.0)*y+(((-7.5)-6.3/x)+y+(-3.4)-y)-((-4.4)-5.0)/(6.3-(-5.4)*(-2.6)/((-y)-5.2/(-y)/x/6.7)+((-y)+(-y)+(-x)*5.4-6.6)))
        //printfn "%d" <| 837
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-6.0)*y+(((-7.5)-6.3/x)+y+(-3.4)-y)-((-4.4)-5.0)/(6.3-(-5.4)*(-2.6)/((-y)-5.2/(-y)/x/6.7)+((-y)+(-y)+(-x)*5.4-6.6)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-6.0)*q+(((-7.5)-6.3/p)+q+(-3.4)-q)-((-4.4)-5.0)/(6.3-(-5.4)*(-2.6)/((-q)-5.2/(-q)/p/6.7)+((-q)+(-q)+(-p)*5.4-6.6)))
            z2 <== ((-6.0)*y+(((-7.5)-6.3/x)+y+(-3.4)-y)-((-4.4)-5.0)/(6.3-(-5.4)*(-2.6)/((-y)-5.2/(-y)/x/6.7)+((-y)+(-y)+(-x)*5.4-6.6)))
            wr.tt <| (I 837)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 838
        ctx.emit.comment "test838"
        //let z0 = (1.1+(-y)-x*1.2/(-4.4)+7.1+y*8.7-(0.5-(y*4.8/(-6.4)+x/4.5))-x+(-6.6)+x-y*(-y))
        //printfn "%d" <| 838
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((1.1+(-y)-x*1.2/(-4.4)+7.1+y*8.7-(0.5-(y*4.8/(-6.4)+x/4.5))-x+(-6.6)+x-y*(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (1.1+(-q)-p*1.2/(-4.4)+7.1+q*8.7-(0.5-(q*4.8/(-6.4)+p/4.5))-p+(-6.6)+p-q*(-q))
            z2 <== (1.1+(-y)-x*1.2/(-4.4)+7.1+y*8.7-(0.5-(y*4.8/(-6.4)+x/4.5))-x+(-6.6)+x-y*(-y))
            wr.tt <| (I 838)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 839
        ctx.emit.comment "test839"
        //let z0 = (((-6.6))-(y+2.2/(-3.0))+(-6.2)-(((-y)-(-y)-2.5/(-y)+y))/(-5.0)/(-x)*(-5.8)*(8.6*y*6.5/6.7)+((-5.1)+(-0.7)+(-3.3)+(-y)*1.7))
        //printfn "%d" <| 839
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-6.6))-(y+2.2/(-3.0))+(-6.2)-(((-y)-(-y)-2.5/(-y)+y))/(-5.0)/(-x)*(-5.8)*(8.6*y*6.5/6.7)+((-5.1)+(-0.7)+(-3.3)+(-y)*1.7))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-6.6))-(q+2.2/(-3.0))+(-6.2)-(((-q)-(-q)-2.5/(-q)+q))/(-5.0)/(-p)*(-5.8)*(8.6*q*6.5/6.7)+((-5.1)+(-0.7)+(-3.3)+(-q)*1.7))
            z2 <== (((-6.6))-(y+2.2/(-3.0))+(-6.2)-(((-y)-(-y)-2.5/(-y)+y))/(-5.0)/(-x)*(-5.8)*(8.6*y*6.5/6.7)+((-5.1)+(-0.7)+(-3.3)+(-y)*1.7))
            wr.tt <| (I 839)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 840
        ctx.emit.comment "test840"
        //let z0 = ((-7.7)+x)
        //printfn "%d" <| 840
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.7)+x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.7)+p)
            z2 <== ((-7.7)+x)
            wr.tt <| (I 840)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 841
        ctx.emit.comment "test841"
        //let z0 = 2.2
        //printfn "%d" <| 841
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 842
        ctx.emit.comment "test842"
        //let z0 = ((-7.4)/4.3+(-y)*((-6.2)))
        //printfn "%d" <| 842
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.4)/4.3+(-y)*((-6.2)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.4)/4.3+(-q)*((-6.2)))
            z2 <== ((-7.4)/4.3+(-y)*((-6.2)))
            wr.tt <| (I 842)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 843
        ctx.emit.comment "test843"
        //let z0 = (-x)
        //printfn "%d" <| 843
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 843)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 844
        ctx.emit.comment "test844"
        //let z0 = y
        //printfn "%d" <| 844
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 844)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 845
        ctx.emit.comment "test845"
        //let z0 = (y/y)
        //printfn "%d" <| 845
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y/y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q/q)
            z2 <== (y/y)
            wr.tt <| (I 845)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 846
        ctx.emit.comment "test846"
        //let z0 = ((-5.2)+(-x))
        //printfn "%d" <| 846
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-5.2)+(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-5.2)+(-p))
            z2 <== ((-5.2)+(-x))
            wr.tt <| (I 846)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 847
        ctx.emit.comment "test847"
        //let z0 = (y)
        //printfn "%d" <| 847
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q)
            z2 <== (y)
            wr.tt <| (I 847)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 848
        ctx.emit.comment "test848"
        //let z0 = y
        //printfn "%d" <| 848
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 848)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 849
        ctx.emit.comment "test849"
        //let z0 = (((-4.1)*(-7.5))*(0.4+y/(-0.6)/3.4-1.2/(-7.4))-1.2-y)
        //printfn "%d" <| 849
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-4.1)*(-7.5))*(0.4+y/(-0.6)/3.4-1.2/(-7.4))-1.2-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-4.1)*(-7.5))*(0.4+q/(-0.6)/3.4-1.2/(-7.4))-1.2-q)
            z2 <== (((-4.1)*(-7.5))*(0.4+y/(-0.6)/3.4-1.2/(-7.4))-1.2-y)
            wr.tt <| (I 849)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 850
        ctx.emit.comment "test850"
        //let z0 = ((6.2/(-y)-(-8.5)-(-y)/0.8*0.5*(y+3.8))-(-0.3)+x-(-x))
        //printfn "%d" <| 850
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((6.2/(-y)-(-8.5)-(-y)/0.8*0.5*(y+3.8))-(-0.3)+x-(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((6.2/(-q)-(-8.5)-(-q)/0.8*0.5*(q+3.8))-(-0.3)+p-(-p))
            z2 <== ((6.2/(-y)-(-8.5)-(-y)/0.8*0.5*(y+3.8))-(-0.3)+x-(-x))
            wr.tt <| (I 850)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 851
        ctx.emit.comment "test851"
        //let z0 = (x-(-1.3)+(((-3.7)+x*4.7*(-x)/(-y))*(x/6.5))/(8.6-y-(-x)))
        //printfn "%d" <| 851
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x-(-1.3)+(((-3.7)+x*4.7*(-x)/(-y))*(x/6.5))/(8.6-y-(-x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p-(-1.3)+(((-3.7)+p*4.7*(-p)/(-q))*(p/6.5))/(8.6-q-(-p)))
            z2 <== (x-(-1.3)+(((-3.7)+x*4.7*(-x)/(-y))*(x/6.5))/(8.6-y-(-x)))
            wr.tt <| (I 851)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 852
        ctx.emit.comment "test852"
        //let z0 = ((y-(-x)-(-5.6))*(-y)*(-3.8))
        //printfn "%d" <| 852
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y-(-x)-(-5.6))*(-y)*(-3.8))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q-(-p)-(-5.6))*(-q)*(-3.8))
            z2 <== ((y-(-x)-(-5.6))*(-y)*(-3.8))
            wr.tt <| (I 852)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 853
        ctx.emit.comment "test853"
        //let z0 = (-1.8)
        //printfn "%d" <| 853
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 854
        ctx.emit.comment "test854"
        //let z0 = (-x)
        //printfn "%d" <| 854
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 854)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 855
        ctx.emit.comment "test855"
        //let z0 = ((x)+(-1.1))
        //printfn "%d" <| 855
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x)+(-1.1))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p)+(-1.1))
            z2 <== ((x)+(-1.1))
            wr.tt <| (I 855)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 856
        ctx.emit.comment "test856"
        //let z0 = (-4.5)
        //printfn "%d" <| 856
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 857
        ctx.emit.comment "test857"
        //let z0 = (-5.4)
        //printfn "%d" <| 857
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 858
        ctx.emit.comment "test858"
        //let z0 = (((-5.3)*7.2-(-6.1))/8.5/y+0.0/y+((-x)*5.3)-2.3-(-5.8)+2.3*1.6)+(-x)
        //printfn "%d" <| 858
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-5.3)*7.2-(-6.1))/8.5/y+0.0/y+((-x)*5.3)-2.3-(-5.8)+2.3*1.6)+(-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-5.3)*7.2-(-6.1))/8.5/q+0.0/q+((-p)*5.3)-2.3-(-5.8)+2.3*1.6)+(-p)
            z2 <== (((-5.3)*7.2-(-6.1))/8.5/y+0.0/y+((-x)*5.3)-2.3-(-5.8)+2.3*1.6)+(-x)
            wr.tt <| (I 858)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 859
        ctx.emit.comment "test859"
        //let z0 = 5.0
        //printfn "%d" <| 859
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 860
        ctx.emit.comment "test860"
        //let z0 = (-7.1)*(((-3.5)))+(-8.4)*(-5.2)/(-y)
        //printfn "%d" <| 860
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-7.1)*(((-3.5)))+(-8.4)*(-5.2)/(-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-7.1)*(((-3.5)))+(-8.4)*(-5.2)/(-q)
            z2 <== (-7.1)*(((-3.5)))+(-8.4)*(-5.2)/(-y)
            wr.tt <| (I 860)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 861
        ctx.emit.comment "test861"
        //let z0 = ((1.0*((-x)+(-3.0)+(-x)*3.3)+(-y)-(8.3*(-y)-y/(-x)))/y+((-y)+(-2.6)+((-5.2))))
        //printfn "%d" <| 861
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((1.0*((-x)+(-3.0)+(-x)*3.3)+(-y)-(8.3*(-y)-y/(-x)))/y+((-y)+(-2.6)+((-5.2))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((1.0*((-p)+(-3.0)+(-p)*3.3)+(-q)-(8.3*(-q)-q/(-p)))/q+((-q)+(-2.6)+((-5.2))))
            z2 <== ((1.0*((-x)+(-3.0)+(-x)*3.3)+(-y)-(8.3*(-y)-y/(-x)))/y+((-y)+(-2.6)+((-5.2))))
            wr.tt <| (I 861)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 862
        ctx.emit.comment "test862"
        //let z0 = (0.6+(-4.3)*7.6/y-(-4.3)+(-x)-(-4.8)-y)*1.1/x
        //printfn "%d" <| 862
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((0.6+(-4.3)*7.6/y-(-4.3)+(-x)-(-4.8)-y)*1.1/x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (0.6+(-4.3)*7.6/q-(-4.3)+(-p)-(-4.8)-q)*1.1/p
            z2 <== (0.6+(-4.3)*7.6/y-(-4.3)+(-x)-(-4.8)-y)*1.1/x
            wr.tt <| (I 862)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 863
        ctx.emit.comment "test863"
        //let z0 = (x*((-0.4)/(-y)-(-x)+(-y))-((-x)+(-2.2)*4.8*y)*8.3+((-3.2))+(-x)-(x+3.8*y-(-y)+(-4.3))/(-x)/(-0.2)/((-y)*2.8*(-5.6)/(-5.8)-(-7.5))-((-3.2)))
        //printfn "%d" <| 863
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x*((-0.4)/(-y)-(-x)+(-y))-((-x)+(-2.2)*4.8*y)*8.3+((-3.2))+(-x)-(x+3.8*y-(-y)+(-4.3))/(-x)/(-0.2)/((-y)*2.8*(-5.6)/(-5.8)-(-7.5))-((-3.2)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p*((-0.4)/(-q)-(-p)+(-q))-((-p)+(-2.2)*4.8*q)*8.3+((-3.2))+(-p)-(p+3.8*q-(-q)+(-4.3))/(-p)/(-0.2)/((-q)*2.8*(-5.6)/(-5.8)-(-7.5))-((-3.2)))
            z2 <== (x*((-0.4)/(-y)-(-x)+(-y))-((-x)+(-2.2)*4.8*y)*8.3+((-3.2))+(-x)-(x+3.8*y-(-y)+(-4.3))/(-x)/(-0.2)/((-y)*2.8*(-5.6)/(-5.8)-(-7.5))-((-3.2)))
            wr.tt <| (I 863)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 864
        ctx.emit.comment "test864"
        //let z0 = 1.2
        //printfn "%d" <| 864
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 865
        ctx.emit.comment "test865"
        //let z0 = ((-y))-((-y)-(-y)+y-(2.6))*(-x)/(6.7*(-y)/y+3.2/(-x)+x+(x+(-x)*(-x)))
        //printfn "%d" <| 865
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y))-((-y)-(-y)+y-(2.6))*(-x)/(6.7*(-y)/y+3.2/(-x)+x+(x+(-x)*(-x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q))-((-q)-(-q)+q-(2.6))*(-p)/(6.7*(-q)/q+3.2/(-p)+p+(p+(-p)*(-p)))
            z2 <== ((-y))-((-y)-(-y)+y-(2.6))*(-x)/(6.7*(-y)/y+3.2/(-x)+x+(x+(-x)*(-x)))
            wr.tt <| (I 865)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 866
        ctx.emit.comment "test866"
        //let z0 = (-5.5)
        //printfn "%d" <| 866
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 867
        ctx.emit.comment "test867"
        //let z0 = (((-y)+(-y)-(-y)))
        //printfn "%d" <| 867
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)+(-y)-(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)+(-q)-(-q)))
            z2 <== (((-y)+(-y)-(-y)))
            wr.tt <| (I 867)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 868
        ctx.emit.comment "test868"
        //let z0 = (-5.4)
        //printfn "%d" <| 868
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 869
        ctx.emit.comment "test869"
        //let z0 = (-y)
        //printfn "%d" <| 869
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 869)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 870
        ctx.emit.comment "test870"
        //let z0 = (1.8-(-3.6)*(-y)-(-x)+(x*(-y)-5.2)/(-y))
        //printfn "%d" <| 870
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((1.8-(-3.6)*(-y)-(-x)+(x*(-y)-5.2)/(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (1.8-(-3.6)*(-q)-(-p)+(p*(-q)-5.2)/(-q))
            z2 <== (1.8-(-3.6)*(-y)-(-x)+(x*(-y)-5.2)/(-y))
            wr.tt <| (I 870)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 871
        ctx.emit.comment "test871"
        //let z0 = (-4.0)+(-8.3)
        //printfn "%d" <| 871
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 872
        ctx.emit.comment "test872"
        //let z0 = y
        //printfn "%d" <| 872
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 872)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 873
        ctx.emit.comment "test873"
        //let z0 = ((-1.8)-(3.8/(-y)-(6.3-(-2.6)/(-y)/(-1.6))*x-y-(-y)))
        //printfn "%d" <| 873
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-1.8)-(3.8/(-y)-(6.3-(-2.6)/(-y)/(-1.6))*x-y-(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-1.8)-(3.8/(-q)-(6.3-(-2.6)/(-q)/(-1.6))*p-q-(-q)))
            z2 <== ((-1.8)-(3.8/(-y)-(6.3-(-2.6)/(-y)/(-1.6))*x-y-(-y)))
            wr.tt <| (I 873)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 874
        ctx.emit.comment "test874"
        //let z0 = ((((-8.8)+(-5.3)-(-8.3)*(-4.2)/(-y))+(-x))*(0.5+x*(-5.7)-x-7.5*(5.7+x+(-4.8)-(-1.7))/(3.5/y)))
        //printfn "%d" <| 874
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-8.8)+(-5.3)-(-8.3)*(-4.2)/(-y))+(-x))*(0.5+x*(-5.7)-x-7.5*(5.7+x+(-4.8)-(-1.7))/(3.5/y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-8.8)+(-5.3)-(-8.3)*(-4.2)/(-q))+(-p))*(0.5+p*(-5.7)-p-7.5*(5.7+p+(-4.8)-(-1.7))/(3.5/q)))
            z2 <== ((((-8.8)+(-5.3)-(-8.3)*(-4.2)/(-y))+(-x))*(0.5+x*(-5.7)-x-7.5*(5.7+x+(-4.8)-(-1.7))/(3.5/y)))
            wr.tt <| (I 874)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 875
        ctx.emit.comment "test875"
        //let z0 = (x/5.2/x+6.6)
        //printfn "%d" <| 875
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x/5.2/x+6.6)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p/5.2/p+6.6)
            z2 <== (x/5.2/x+6.6)
            wr.tt <| (I 875)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 876
        ctx.emit.comment "test876"
        //let z0 = (-4.7)
        //printfn "%d" <| 876
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 877
        ctx.emit.comment "test877"
        //let z0 = 6.3
        //printfn "%d" <| 877
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 878
        ctx.emit.comment "test878"
        //let z0 = 7.1
        //printfn "%d" <| 878
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 879
        ctx.emit.comment "test879"
        //let z0 = y
        //printfn "%d" <| 879
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 879)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 880
        ctx.emit.comment "test880"
        //let z0 = 8.7
        //printfn "%d" <| 880
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 881
        ctx.emit.comment "test881"
        //let z0 = ((-y))
        //printfn "%d" <| 881
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q))
            z2 <== ((-y))
            wr.tt <| (I 881)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 882
        ctx.emit.comment "test882"
        //let z0 = y+(-y)
        //printfn "%d" <| 882
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y+(-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q+(-q)
            z2 <== y+(-y)
            wr.tt <| (I 882)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 883
        ctx.emit.comment "test883"
        //let z0 = (((-2.2))+(y-5.5-(-5.4)*3.3+(-y)))
        //printfn "%d" <| 883
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-2.2))+(y-5.5-(-5.4)*3.3+(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-2.2))+(q-5.5-(-5.4)*3.3+(-q)))
            z2 <== (((-2.2))+(y-5.5-(-5.4)*3.3+(-y)))
            wr.tt <| (I 883)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 884
        ctx.emit.comment "test884"
        //let z0 = (((-4.4)+(-0.0)*(0.7+(-5.3)+(-4.1)/x+0.6)))
        //printfn "%d" <| 884
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-4.4)+(-0.0)*(0.7+(-5.3)+(-4.1)/x+0.6)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-4.4)+(-0.0)*(0.7+(-5.3)+(-4.1)/p+0.6)))
            z2 <== (((-4.4)+(-0.0)*(0.7+(-5.3)+(-4.1)/x+0.6)))
            wr.tt <| (I 884)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 885
        ctx.emit.comment "test885"
        //let z0 = (((-1.6))-(-x)*(4.6-(-y)*1.5/(-1.7)+(-x))*(((-0.5)+(-6.4)*8.0+(-7.6)*y)*((-y))+(y+y+(-x)-(-y)/(-x))))
        //printfn "%d" <| 885
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-1.6))-(-x)*(4.6-(-y)*1.5/(-1.7)+(-x))*(((-0.5)+(-6.4)*8.0+(-7.6)*y)*((-y))+(y+y+(-x)-(-y)/(-x))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-1.6))-(-p)*(4.6-(-q)*1.5/(-1.7)+(-p))*(((-0.5)+(-6.4)*8.0+(-7.6)*q)*((-q))+(q+q+(-p)-(-q)/(-p))))
            z2 <== (((-1.6))-(-x)*(4.6-(-y)*1.5/(-1.7)+(-x))*(((-0.5)+(-6.4)*8.0+(-7.6)*y)*((-y))+(y+y+(-x)-(-y)/(-x))))
            wr.tt <| (I 885)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 886
        ctx.emit.comment "test886"
        //let z0 = (1.3-((-1.3))-x+y-(-6.0)*(-x))*y/(((-x)+x/8.5/(-x)))*x-2.0
        //printfn "%d" <| 886
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((1.3-((-1.3))-x+y-(-6.0)*(-x))*y/(((-x)+x/8.5/(-x)))*x-2.0).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (1.3-((-1.3))-p+q-(-6.0)*(-p))*q/(((-p)+p/8.5/(-p)))*p-2.0
            z2 <== (1.3-((-1.3))-x+y-(-6.0)*(-x))*y/(((-x)+x/8.5/(-x)))*x-2.0
            wr.tt <| (I 886)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 887
        ctx.emit.comment "test887"
        //let z0 = (((-y)/(-y))*(((-x)*(-x)-(-x)-(-x)+(-7.3))-x-(-y)+y-x*(-x))/(y/(x+0.0/3.3)+(4.1+y/(-6.1)-(-y)-(-y))*(-6.4))*(((-x))-y+5.7-(-2.8)/(-y))-6.2*(y+6.6)-(-y)-((-0.1)*y))
        //printfn "%d" <| 887
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)/(-y))*(((-x)*(-x)-(-x)-(-x)+(-7.3))-x-(-y)+y-x*(-x))/(y/(x+0.0/3.3)+(4.1+y/(-6.1)-(-y)-(-y))*(-6.4))*(((-x))-y+5.7-(-2.8)/(-y))-6.2*(y+6.6)-(-y)-((-0.1)*y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)/(-q))*(((-p)*(-p)-(-p)-(-p)+(-7.3))-p-(-q)+q-p*(-p))/(q/(p+0.0/3.3)+(4.1+q/(-6.1)-(-q)-(-q))*(-6.4))*(((-p))-q+5.7-(-2.8)/(-q))-6.2*(q+6.6)-(-q)-((-0.1)*q))
            z2 <== (((-y)/(-y))*(((-x)*(-x)-(-x)-(-x)+(-7.3))-x-(-y)+y-x*(-x))/(y/(x+0.0/3.3)+(4.1+y/(-6.1)-(-y)-(-y))*(-6.4))*(((-x))-y+5.7-(-2.8)/(-y))-6.2*(y+6.6)-(-y)-((-0.1)*y))
            wr.tt <| (I 887)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 888
        ctx.emit.comment "test888"
        //let z0 = 2.3
        //printfn "%d" <| 888
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 889
        ctx.emit.comment "test889"
        //let z0 = x
        //printfn "%d" <| 889
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 889)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 890
        ctx.emit.comment "test890"
        //let z0 = (-3.5)
        //printfn "%d" <| 890
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 891
        ctx.emit.comment "test891"
        //let z0 = (((-x)+y/(-5.1)/(-8.5)-(-y)*y-((-y)*0.1))/(-y))
        //printfn "%d" <| 891
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)+y/(-5.1)/(-8.5)-(-y)*y-((-y)*0.1))/(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)+q/(-5.1)/(-8.5)-(-q)*q-((-q)*0.1))/(-q))
            z2 <== (((-x)+y/(-5.1)/(-8.5)-(-y)*y-((-y)*0.1))/(-y))
            wr.tt <| (I 891)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 892
        ctx.emit.comment "test892"
        //let z0 = (((-6.4)-x+(-x)*(1.1*(-y)-2.6/3.2/1.5))+(-y)/3.1)
        //printfn "%d" <| 892
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-6.4)-x+(-x)*(1.1*(-y)-2.6/3.2/1.5))+(-y)/3.1)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-6.4)-p+(-p)*(1.1*(-q)-2.6/3.2/1.5))+(-q)/3.1)
            z2 <== (((-6.4)-x+(-x)*(1.1*(-y)-2.6/3.2/1.5))+(-y)/3.1)
            wr.tt <| (I 892)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 893
        ctx.emit.comment "test893"
        //let z0 = (y+2.1+x)
        //printfn "%d" <| 893
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+2.1+x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+2.1+p)
            z2 <== (y+2.1+x)
            wr.tt <| (I 893)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 894
        ctx.emit.comment "test894"
        //let z0 = ((-2.7)/0.0-0.1-y)+y+(-2.4)*1.5+(-y)-(-y)
        //printfn "%d" <| 894
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-2.7)/0.0-0.1-y)+y+(-2.4)*1.5+(-y)-(-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-2.7)/0.0-0.1-q)+q+(-2.4)*1.5+(-q)-(-q)
            z2 <== ((-2.7)/0.0-0.1-y)+y+(-2.4)*1.5+(-y)-(-y)
            wr.tt <| (I 894)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 895
        ctx.emit.comment "test895"
        //let z0 = (-6.1)+(-y)+(-0.5)
        //printfn "%d" <| 895
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-6.1)+(-y)+(-0.5)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-6.1)+(-q)+(-0.5)
            z2 <== (-6.1)+(-y)+(-0.5)
            wr.tt <| (I 895)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 896
        ctx.emit.comment "test896"
        //let z0 = ((((-x)/(-y)/(-1.8))/x+2.5+(-1.4)+(-x)*y/((-4.4)*(-y)+(-y)-(-y)*y)/(-3.6)))
        //printfn "%d" <| 896
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-x)/(-y)/(-1.8))/x+2.5+(-1.4)+(-x)*y/((-4.4)*(-y)+(-y)-(-y)*y)/(-3.6)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-p)/(-q)/(-1.8))/p+2.5+(-1.4)+(-p)*q/((-4.4)*(-q)+(-q)-(-q)*q)/(-3.6)))
            z2 <== ((((-x)/(-y)/(-1.8))/x+2.5+(-1.4)+(-x)*y/((-4.4)*(-y)+(-y)-(-y)*y)/(-3.6)))
            wr.tt <| (I 896)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 897
        ctx.emit.comment "test897"
        //let z0 = (-7.2)
        //printfn "%d" <| 897
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 898
        ctx.emit.comment "test898"
        //let z0 = 3.3
        //printfn "%d" <| 898
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 899
        ctx.emit.comment "test899"
        //let z0 = ((5.6))
        //printfn "%d" <| 899
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 900
        ctx.emit.comment "test900"
        //let z0 = (((-x)-y-(8.4+0.6-(-x)))*(((-0.7)-4.0)*(-4.2))/(0.0-(-6.6)/y/y)/(-7.8)/((-1.7)*x/(-6.7)*8.4/(-0.8)-4.0-(-0.5)))
        //printfn "%d" <| 900
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)-y-(8.4+0.6-(-x)))*(((-0.7)-4.0)*(-4.2))/(0.0-(-6.6)/y/y)/(-7.8)/((-1.7)*x/(-6.7)*8.4/(-0.8)-4.0-(-0.5)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)-q-(8.4+0.6-(-p)))*(((-0.7)-4.0)*(-4.2))/(0.0-(-6.6)/q/q)/(-7.8)/((-1.7)*p/(-6.7)*8.4/(-0.8)-4.0-(-0.5)))
            z2 <== (((-x)-y-(8.4+0.6-(-x)))*(((-0.7)-4.0)*(-4.2))/(0.0-(-6.6)/y/y)/(-7.8)/((-1.7)*x/(-6.7)*8.4/(-0.8)-4.0-(-0.5)))
            wr.tt <| (I 900)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 901
        ctx.emit.comment "test901"
        //let z0 = x-(-2.0)
        //printfn "%d" <| 901
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x-(-2.0)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p-(-2.0)
            z2 <== x-(-2.0)
            wr.tt <| (I 901)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 902
        ctx.emit.comment "test902"
        //let z0 = x
        //printfn "%d" <| 902
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 902)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 903
        ctx.emit.comment "test903"
        //let z0 = (-6.6)
        //printfn "%d" <| 903
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 904
        ctx.emit.comment "test904"
        //let z0 = y
        //printfn "%d" <| 904
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 904)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 905
        ctx.emit.comment "test905"
        //let z0 = y
        //printfn "%d" <| 905
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 905)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 906
        ctx.emit.comment "test906"
        //let z0 = (-8.4)
        //printfn "%d" <| 906
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 907
        ctx.emit.comment "test907"
        //let z0 = 4.1*(-y)/(-x)/(x+(-2.3)*x*7.7)/(5.4+(-y)-1.1+(-6.5)/y)/x+x+3.2/(-1.0)
        //printfn "%d" <| 907
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (4.1*(-y)/(-x)/(x+(-2.3)*x*7.7)/(5.4+(-y)-1.1+(-6.5)/y)/x+x+3.2/(-1.0)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 4.1*(-q)/(-p)/(p+(-2.3)*p*7.7)/(5.4+(-q)-1.1+(-6.5)/q)/p+p+3.2/(-1.0)
            z2 <== 4.1*(-y)/(-x)/(x+(-2.3)*x*7.7)/(5.4+(-y)-1.1+(-6.5)/y)/x+x+3.2/(-1.0)
            wr.tt <| (I 907)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 908
        ctx.emit.comment "test908"
        //let z0 = (-x)
        //printfn "%d" <| 908
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 908)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 909
        ctx.emit.comment "test909"
        //let z0 = (x-(((-y)/0.1*x)*(-6.2)))
        //printfn "%d" <| 909
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x-(((-y)/0.1*x)*(-6.2)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p-(((-q)/0.1*p)*(-6.2)))
            z2 <== (x-(((-y)/0.1*x)*(-6.2)))
            wr.tt <| (I 909)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 910
        ctx.emit.comment "test910"
        //let z0 = (-x)+x*(-3.5)/y/(-3.1)+y/y-(((-x)))/(-y)-((x)/(7.7/8.7/4.4))
        //printfn "%d" <| 910
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)+x*(-3.5)/y/(-3.1)+y/y-(((-x)))/(-y)-((x)/(7.7/8.7/4.4))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)+p*(-3.5)/q/(-3.1)+q/q-(((-p)))/(-q)-((p)/(7.7/8.7/4.4))
            z2 <== (-x)+x*(-3.5)/y/(-3.1)+y/y-(((-x)))/(-y)-((x)/(7.7/8.7/4.4))
            wr.tt <| (I 910)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 911
        ctx.emit.comment "test911"
        //let z0 = y
        //printfn "%d" <| 911
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 911)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 912
        ctx.emit.comment "test912"
        //let z0 = (-4.1)
        //printfn "%d" <| 912
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 913
        ctx.emit.comment "test913"
        //let z0 = ((-0.6)+(-y)*(-y)+(-0.2))
        //printfn "%d" <| 913
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.6)+(-y)*(-y)+(-0.2))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.6)+(-q)*(-q)+(-0.2))
            z2 <== ((-0.6)+(-y)*(-y)+(-0.2))
            wr.tt <| (I 913)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 914
        ctx.emit.comment "test914"
        //let z0 = ((-4.5)-1.3+(y-((-1.4)/x/x-(-y)*(-x))*(-5.8)-(1.5*1.8-(-x)-4.6))+(x-(-x)+x*(-x)))
        //printfn "%d" <| 914
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-4.5)-1.3+(y-((-1.4)/x/x-(-y)*(-x))*(-5.8)-(1.5*1.8-(-x)-4.6))+(x-(-x)+x*(-x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-4.5)-1.3+(q-((-1.4)/p/p-(-q)*(-p))*(-5.8)-(1.5*1.8-(-p)-4.6))+(p-(-p)+p*(-p)))
            z2 <== ((-4.5)-1.3+(y-((-1.4)/x/x-(-y)*(-x))*(-5.8)-(1.5*1.8-(-x)-4.6))+(x-(-x)+x*(-x)))
            wr.tt <| (I 914)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 915
        ctx.emit.comment "test915"
        //let z0 = (x/(y+7.3-(-y))/1.7+x/8.4/(x+(-6.6)-((-4.5)-y-y*(-8.2))))
        //printfn "%d" <| 915
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x/(y+7.3-(-y))/1.7+x/8.4/(x+(-6.6)-((-4.5)-y-y*(-8.2))))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p/(q+7.3-(-q))/1.7+p/8.4/(p+(-6.6)-((-4.5)-q-q*(-8.2))))
            z2 <== (x/(y+7.3-(-y))/1.7+x/8.4/(x+(-6.6)-((-4.5)-y-y*(-8.2))))
            wr.tt <| (I 915)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 916
        ctx.emit.comment "test916"
        //let z0 = ((3.1/(-x)*(-x)/(-2.8)/(-1.5))/x)-(-0.3)*(((-3.3)-7.2*(-3.5))-y-(8.0/(-0.5)/y))
        //printfn "%d" <| 916
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((3.1/(-x)*(-x)/(-2.8)/(-1.5))/x)-(-0.3)*(((-3.3)-7.2*(-3.5))-y-(8.0/(-0.5)/y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((3.1/(-p)*(-p)/(-2.8)/(-1.5))/p)-(-0.3)*(((-3.3)-7.2*(-3.5))-q-(8.0/(-0.5)/q))
            z2 <== ((3.1/(-x)*(-x)/(-2.8)/(-1.5))/x)-(-0.3)*(((-3.3)-7.2*(-3.5))-y-(8.0/(-0.5)/y))
            wr.tt <| (I 916)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 917
        ctx.emit.comment "test917"
        //let z0 = ((1.5-(-2.0)/(-y))*(-y)-((-x)))
        //printfn "%d" <| 917
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((1.5-(-2.0)/(-y))*(-y)-((-x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((1.5-(-2.0)/(-q))*(-q)-((-p)))
            z2 <== ((1.5-(-2.0)/(-y))*(-y)-((-x)))
            wr.tt <| (I 917)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 918
        ctx.emit.comment "test918"
        //let z0 = (((-y)-y/y+x/(-3.2)*(-x)*x))
        //printfn "%d" <| 918
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)-y/y+x/(-3.2)*(-x)*x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)-q/q+p/(-3.2)*(-p)*p))
            z2 <== (((-y)-y/y+x/(-3.2)*(-x)*x))
            wr.tt <| (I 918)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 919
        ctx.emit.comment "test919"
        //let z0 = 2.8
        //printfn "%d" <| 919
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 920
        ctx.emit.comment "test920"
        //let z0 = y
        //printfn "%d" <| 920
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 920)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 921
        ctx.emit.comment "test921"
        //let z0 = (-y)
        //printfn "%d" <| 921
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 921)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 922
        ctx.emit.comment "test922"
        //let z0 = (-x)
        //printfn "%d" <| 922
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 922)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 923
        ctx.emit.comment "test923"
        //let z0 = x
        //printfn "%d" <| 923
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 923)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 924
        ctx.emit.comment "test924"
        //let z0 = (-1.0)
        //printfn "%d" <| 924
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 925
        ctx.emit.comment "test925"
        //let z0 = 5.1
        //printfn "%d" <| 925
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 926
        ctx.emit.comment "test926"
        //let z0 = (x+(-8.3)-(-0.3)/7.3)+x*(7.7)
        //printfn "%d" <| 926
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x+(-8.3)-(-0.3)/7.3)+x*(7.7)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p+(-8.3)-(-0.3)/7.3)+p*(7.7)
            z2 <== (x+(-8.3)-(-0.3)/7.3)+x*(7.7)
            wr.tt <| (I 926)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 927
        ctx.emit.comment "test927"
        //let z0 = (-x)
        //printfn "%d" <| 927
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 927)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 928
        ctx.emit.comment "test928"
        //let z0 = (-0.7)
        //printfn "%d" <| 928
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 929
        ctx.emit.comment "test929"
        //let z0 = (-y)
        //printfn "%d" <| 929
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 929)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 930
        ctx.emit.comment "test930"
        //let z0 = (y+(x/(y-(-7.0))-1.0-(y*4.3))-7.1)
        //printfn "%d" <| 930
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+(x/(y-(-7.0))-1.0-(y*4.3))-7.1)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+(p/(q-(-7.0))-1.0-(q*4.3))-7.1)
            z2 <== (y+(x/(y-(-7.0))-1.0-(y*4.3))-7.1)
            wr.tt <| (I 930)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 931
        ctx.emit.comment "test931"
        //let z0 = y
        //printfn "%d" <| 931
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 931)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 932
        ctx.emit.comment "test932"
        //let z0 = ((-2.7))
        //printfn "%d" <| 932
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 933
        ctx.emit.comment "test933"
        //let z0 = (-4.6)
        //printfn "%d" <| 933
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 934
        ctx.emit.comment "test934"
        //let z0 = ((((-3.4)+x-4.8+x)-(-x)/(3.3/3.5-(-y)/1.3)+(-y))-((-x)/((-5.7)-(-1.3)*x-(-2.3)*6.8)*(5.4+x-(-5.5)+(-y)))+(-x))
        //printfn "%d" <| 934
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-3.4)+x-4.8+x)-(-x)/(3.3/3.5-(-y)/1.3)+(-y))-((-x)/((-5.7)-(-1.3)*x-(-2.3)*6.8)*(5.4+x-(-5.5)+(-y)))+(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-3.4)+p-4.8+p)-(-p)/(3.3/3.5-(-q)/1.3)+(-q))-((-p)/((-5.7)-(-1.3)*p-(-2.3)*6.8)*(5.4+p-(-5.5)+(-q)))+(-p))
            z2 <== ((((-3.4)+x-4.8+x)-(-x)/(3.3/3.5-(-y)/1.3)+(-y))-((-x)/((-5.7)-(-1.3)*x-(-2.3)*6.8)*(5.4+x-(-5.5)+(-y)))+(-x))
            wr.tt <| (I 934)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 935
        ctx.emit.comment "test935"
        //let z0 = (((-y)+(-7.1)*(-1.6))/8.8)
        //printfn "%d" <| 935
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)+(-7.1)*(-1.6))/8.8)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)+(-7.1)*(-1.6))/8.8)
            z2 <== (((-y)+(-7.1)*(-1.6))/8.8)
            wr.tt <| (I 935)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 936
        ctx.emit.comment "test936"
        //let z0 = (-7.7)
        //printfn "%d" <| 936
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 937
        ctx.emit.comment "test937"
        //let z0 = (-1.8)
        //printfn "%d" <| 937
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 938
        ctx.emit.comment "test938"
        //let z0 = 0.8
        //printfn "%d" <| 938
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 939
        ctx.emit.comment "test939"
        //let z0 = (y)
        //printfn "%d" <| 939
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q)
            z2 <== (y)
            wr.tt <| (I 939)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 940
        ctx.emit.comment "test940"
        //let z0 = (-1.8)
        //printfn "%d" <| 940
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 941
        ctx.emit.comment "test941"
        //let z0 = (2.4)
        //printfn "%d" <| 941
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 942
        ctx.emit.comment "test942"
        //let z0 = ((-0.7)+(-1.6)-2.7+(-y)-x)
        //printfn "%d" <| 942
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.7)+(-1.6)-2.7+(-y)-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.7)+(-1.6)-2.7+(-q)-p)
            z2 <== ((-0.7)+(-1.6)-2.7+(-y)-x)
            wr.tt <| (I 942)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 943
        ctx.emit.comment "test943"
        //let z0 = (y+(-7.8)*((y/y/(-y)+y+3.1)-x*((-x))*(x-x))-(-x)-(-x))
        //printfn "%d" <| 943
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+(-7.8)*((y/y/(-y)+y+3.1)-x*((-x))*(x-x))-(-x)-(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+(-7.8)*((q/q/(-q)+q+3.1)-p*((-p))*(p-p))-(-p)-(-p))
            z2 <== (y+(-7.8)*((y/y/(-y)+y+3.1)-x*((-x))*(x-x))-(-x)-(-x))
            wr.tt <| (I 943)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 944
        ctx.emit.comment "test944"
        //let z0 = (((-y)/(-x)+((-y)))/(-6.3)-((y+2.2-4.8+6.4*(-2.2))*y))
        //printfn "%d" <| 944
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)/(-x)+((-y)))/(-6.3)-((y+2.2-4.8+6.4*(-2.2))*y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)/(-p)+((-q)))/(-6.3)-((q+2.2-4.8+6.4*(-2.2))*q))
            z2 <== (((-y)/(-x)+((-y)))/(-6.3)-((y+2.2-4.8+6.4*(-2.2))*y))
            wr.tt <| (I 944)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 945
        ctx.emit.comment "test945"
        //let z0 = 4.8-((2.2-(-y)-3.4/4.4+y)+y-y/((-2.3)-(-y)/8.1-(-2.2)))+((-y)+x)
        //printfn "%d" <| 945
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (4.8-((2.2-(-y)-3.4/4.4+y)+y-y/((-2.3)-(-y)/8.1-(-2.2)))+((-y)+x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 4.8-((2.2-(-q)-3.4/4.4+q)+q-q/((-2.3)-(-q)/8.1-(-2.2)))+((-q)+p)
            z2 <== 4.8-((2.2-(-y)-3.4/4.4+y)+y-y/((-2.3)-(-y)/8.1-(-2.2)))+((-y)+x)
            wr.tt <| (I 945)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 946
        ctx.emit.comment "test946"
        //let z0 = y
        //printfn "%d" <| 946
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 946)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 947
        ctx.emit.comment "test947"
        //let z0 = ((-4.8)/((y-(-y)/(-x)*(-1.7)/(-x))-(-x)+6.5)-1.1/(((-x)*(-y)/1.7*(-2.2))*4.2+1.1/y+y+y-x*3.5))
        //printfn "%d" <| 947
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-4.8)/((y-(-y)/(-x)*(-1.7)/(-x))-(-x)+6.5)-1.1/(((-x)*(-y)/1.7*(-2.2))*4.2+1.1/y+y+y-x*3.5))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-4.8)/((q-(-q)/(-p)*(-1.7)/(-p))-(-p)+6.5)-1.1/(((-p)*(-q)/1.7*(-2.2))*4.2+1.1/q+q+q-p*3.5))
            z2 <== ((-4.8)/((y-(-y)/(-x)*(-1.7)/(-x))-(-x)+6.5)-1.1/(((-x)*(-y)/1.7*(-2.2))*4.2+1.1/y+y+y-x*3.5))
            wr.tt <| (I 947)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 948
        ctx.emit.comment "test948"
        //let z0 = (-7.1)
        //printfn "%d" <| 948
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 949
        ctx.emit.comment "test949"
        //let z0 = (-x)
        //printfn "%d" <| 949
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 949)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 950
        ctx.emit.comment "test950"
        //let z0 = (-7.1)
        //printfn "%d" <| 950
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 951
        ctx.emit.comment "test951"
        //let z0 = x
        //printfn "%d" <| 951
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 951)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 952
        ctx.emit.comment "test952"
        //let z0 = 2.7
        //printfn "%d" <| 952
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 953
        ctx.emit.comment "test953"
        //let z0 = (-5.8)
        //printfn "%d" <| 953
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 954
        ctx.emit.comment "test954"
        //let z0 = (-y)
        //printfn "%d" <| 954
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 954)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 955
        ctx.emit.comment "test955"
        //let z0 = (-1.1)
        //printfn "%d" <| 955
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 956
        ctx.emit.comment "test956"
        //let z0 = (-x)/3.6
        //printfn "%d" <| 956
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)/3.6).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)/3.6
            z2 <== (-x)/3.6
            wr.tt <| (I 956)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 957
        ctx.emit.comment "test957"
        //let z0 = (0.1+(-x))
        //printfn "%d" <| 957
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((0.1+(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (0.1+(-p))
            z2 <== (0.1+(-x))
            wr.tt <| (I 957)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 958
        ctx.emit.comment "test958"
        //let z0 = x
        //printfn "%d" <| 958
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 958)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 959
        ctx.emit.comment "test959"
        //let z0 = ((((-y)-x)+((-y)/(-0.2)/(-4.7))-(-x)+y))
        //printfn "%d" <| 959
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-y)-x)+((-y)/(-0.2)/(-4.7))-(-x)+y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-q)-p)+((-q)/(-0.2)/(-4.7))-(-p)+q))
            z2 <== ((((-y)-x)+((-y)/(-0.2)/(-4.7))-(-x)+y))
            wr.tt <| (I 959)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 960
        ctx.emit.comment "test960"
        //let z0 = (-y)
        //printfn "%d" <| 960
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 960)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 961
        ctx.emit.comment "test961"
        //let z0 = 7.2
        //printfn "%d" <| 961
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 962
        ctx.emit.comment "test962"
        //let z0 = 4.5
        //printfn "%d" <| 962
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 963
        ctx.emit.comment "test963"
        //let z0 = (y-y)
        //printfn "%d" <| 963
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q-q)
            z2 <== (y-y)
            wr.tt <| (I 963)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 964
        ctx.emit.comment "test964"
        //let z0 = ((0.1+(-8.2)/(-4.6)+y+8.2))/(y*(-4.5))-(-2.4)/(y)-(x)+x
        //printfn "%d" <| 964
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((0.1+(-8.2)/(-4.6)+y+8.2))/(y*(-4.5))-(-2.4)/(y)-(x)+x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((0.1+(-8.2)/(-4.6)+q+8.2))/(q*(-4.5))-(-2.4)/(q)-(p)+p
            z2 <== ((0.1+(-8.2)/(-4.6)+y+8.2))/(y*(-4.5))-(-2.4)/(y)-(x)+x
            wr.tt <| (I 964)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 965
        ctx.emit.comment "test965"
        //let z0 = (-4.1)
        //printfn "%d" <| 965
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 966
        ctx.emit.comment "test966"
        //let z0 = (((-x)/(4.8/(-x))+x/(-x))*(-5.2)-(((-y)-(-5.3))*3.7-x*(y*y*y)*x))
        //printfn "%d" <| 966
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)/(4.8/(-x))+x/(-x))*(-5.2)-(((-y)-(-5.3))*3.7-x*(y*y*y)*x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)/(4.8/(-p))+p/(-p))*(-5.2)-(((-q)-(-5.3))*3.7-p*(q*q*q)*p))
            z2 <== (((-x)/(4.8/(-x))+x/(-x))*(-5.2)-(((-y)-(-5.3))*3.7-x*(y*y*y)*x))
            wr.tt <| (I 966)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 967
        ctx.emit.comment "test967"
        //let z0 = (-2.0)
        //printfn "%d" <| 967
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 968
        ctx.emit.comment "test968"
        //let z0 = (-5.4)-x+((y*y*(-x)+(-5.8))*((-x)+(-6.3)-6.1))/((-5.1)/(-0.5)+4.1/(-x))/((-y)*1.5/((-2.6)/2.6*y))
        //printfn "%d" <| 968
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-5.4)-x+((y*y*(-x)+(-5.8))*((-x)+(-6.3)-6.1))/((-5.1)/(-0.5)+4.1/(-x))/((-y)*1.5/((-2.6)/2.6*y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-5.4)-p+((q*q*(-p)+(-5.8))*((-p)+(-6.3)-6.1))/((-5.1)/(-0.5)+4.1/(-p))/((-q)*1.5/((-2.6)/2.6*q))
            z2 <== (-5.4)-x+((y*y*(-x)+(-5.8))*((-x)+(-6.3)-6.1))/((-5.1)/(-0.5)+4.1/(-x))/((-y)*1.5/((-2.6)/2.6*y))
            wr.tt <| (I 968)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 969
        ctx.emit.comment "test969"
        //let z0 = (5.3-(-2.8)/(-4.4)/((-2.8)-(3.4*x)*(6.4))+2.3)
        //printfn "%d" <| 969
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.3-(-2.8)/(-4.4)/((-2.8)-(3.4*x)*(6.4))+2.3)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.3-(-2.8)/(-4.4)/((-2.8)-(3.4*p)*(6.4))+2.3)
            z2 <== (5.3-(-2.8)/(-4.4)/((-2.8)-(3.4*x)*(6.4))+2.3)
            wr.tt <| (I 969)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 970
        ctx.emit.comment "test970"
        //let z0 = ((-8.2)/(x/8.2-(-x)-(-x)+(-y)*y/y/(-x))*((x)/(y*(-y)/y-(-5.1))+8.7/((-y)-x)/7.8)-(((-y)/8.4*y)-(-0.8)*((-x)-(-5.4)-8.0-(-8.7)))*(-2.6))
        //printfn "%d" <| 970
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-8.2)/(x/8.2-(-x)-(-x)+(-y)*y/y/(-x))*((x)/(y*(-y)/y-(-5.1))+8.7/((-y)-x)/7.8)-(((-y)/8.4*y)-(-0.8)*((-x)-(-5.4)-8.0-(-8.7)))*(-2.6))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-8.2)/(p/8.2-(-p)-(-p)+(-q)*q/q/(-p))*((p)/(q*(-q)/q-(-5.1))+8.7/((-q)-p)/7.8)-(((-q)/8.4*q)-(-0.8)*((-p)-(-5.4)-8.0-(-8.7)))*(-2.6))
            z2 <== ((-8.2)/(x/8.2-(-x)-(-x)+(-y)*y/y/(-x))*((x)/(y*(-y)/y-(-5.1))+8.7/((-y)-x)/7.8)-(((-y)/8.4*y)-(-0.8)*((-x)-(-5.4)-8.0-(-8.7)))*(-2.6))
            wr.tt <| (I 970)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 971
        ctx.emit.comment "test971"
        //let z0 = (((-y)+(1.8*0.5))-(-y)*4.6-y/(x+(1.2-(-x)-(-2.6)-(-y))+(1.5)*(-y)))
        //printfn "%d" <| 971
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)+(1.8*0.5))-(-y)*4.6-y/(x+(1.2-(-x)-(-2.6)-(-y))+(1.5)*(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)+(1.8*0.5))-(-q)*4.6-q/(p+(1.2-(-p)-(-2.6)-(-q))+(1.5)*(-q)))
            z2 <== (((-y)+(1.8*0.5))-(-y)*4.6-y/(x+(1.2-(-x)-(-2.6)-(-y))+(1.5)*(-y)))
            wr.tt <| (I 971)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 972
        ctx.emit.comment "test972"
        //let z0 = x
        //printfn "%d" <| 972
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 972)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 973
        ctx.emit.comment "test973"
        //let z0 = x
        //printfn "%d" <| 973
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 973)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 974
        ctx.emit.comment "test974"
        //let z0 = 2.3+(8.1+y-5.5/(-7.6)*(x/x)/((-1.6)/y*x/y))
        //printfn "%d" <| 974
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (2.3+(8.1+y-5.5/(-7.6)*(x/x)/((-1.6)/y*x/y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 2.3+(8.1+q-5.5/(-7.6)*(p/p)/((-1.6)/q*p/q))
            z2 <== 2.3+(8.1+y-5.5/(-7.6)*(x/x)/((-1.6)/y*x/y))
            wr.tt <| (I 974)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 975
        ctx.emit.comment "test975"
        //let z0 = ((-1.2)/(4.3)/(-x))
        //printfn "%d" <| 975
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-1.2)/(4.3)/(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-1.2)/(4.3)/(-p))
            z2 <== ((-1.2)/(4.3)/(-x))
            wr.tt <| (I 975)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 976
        ctx.emit.comment "test976"
        //let z0 = (-y)
        //printfn "%d" <| 976
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 976)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 977
        ctx.emit.comment "test977"
        //let z0 = x
        //printfn "%d" <| 977
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 977)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 978
        ctx.emit.comment "test978"
        //let z0 = 0.4
        //printfn "%d" <| 978
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 979
        ctx.emit.comment "test979"
        //let z0 = y
        //printfn "%d" <| 979
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 979)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 980
        ctx.emit.comment "test980"
        //let z0 = ((x*y-1.3*(-6.2))*(-7.5)/(-6.7))
        //printfn "%d" <| 980
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x*y-1.3*(-6.2))*(-7.5)/(-6.7))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p*q-1.3*(-6.2))*(-7.5)/(-6.7))
            z2 <== ((x*y-1.3*(-6.2))*(-7.5)/(-6.7))
            wr.tt <| (I 980)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 981
        ctx.emit.comment "test981"
        //let z0 = (-2.7)
        //printfn "%d" <| 981
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 982
        ctx.emit.comment "test982"
        //let z0 = (-y)
        //printfn "%d" <| 982
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr.tt <| (I 982)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 983
        ctx.emit.comment "test983"
        //let z0 = 0.1
        //printfn "%d" <| 983
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 984
        ctx.emit.comment "test984"
        //let z0 = x
        //printfn "%d" <| 984
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr.tt <| (I 984)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 985
        ctx.emit.comment "test985"
        //let z0 = (-x)
        //printfn "%d" <| 985
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 985)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 986
        ctx.emit.comment "test986"
        //let z0 = (((-5.0)/(-x)+x/3.3*(-y))*(0.1+(-6.2))-((-5.7)*(-y))*(-x))
        //printfn "%d" <| 986
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-5.0)/(-x)+x/3.3*(-y))*(0.1+(-6.2))-((-5.7)*(-y))*(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-5.0)/(-p)+p/3.3*(-q))*(0.1+(-6.2))-((-5.7)*(-q))*(-p))
            z2 <== (((-5.0)/(-x)+x/3.3*(-y))*(0.1+(-6.2))-((-5.7)*(-y))*(-x))
            wr.tt <| (I 986)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 987
        ctx.emit.comment "test987"
        //let z0 = ((-7.3)+((-1.0)/y/(-y)+(-x))-((-y)-8.2)*((-x)*(-y)-(-y))+6.8*((y/6.2)+(-3.7)+(x*x-7.6+(-5.3))*x/(-5.0)*(-x)-(-0.6)+(-4.4))+((5.3)/(y-y)-(-y)))
        //printfn "%d" <| 987
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.3)+((-1.0)/y/(-y)+(-x))-((-y)-8.2)*((-x)*(-y)-(-y))+6.8*((y/6.2)+(-3.7)+(x*x-7.6+(-5.3))*x/(-5.0)*(-x)-(-0.6)+(-4.4))+((5.3)/(y-y)-(-y)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.3)+((-1.0)/q/(-q)+(-p))-((-q)-8.2)*((-p)*(-q)-(-q))+6.8*((q/6.2)+(-3.7)+(p*p-7.6+(-5.3))*p/(-5.0)*(-p)-(-0.6)+(-4.4))+((5.3)/(q-q)-(-q)))
            z2 <== ((-7.3)+((-1.0)/y/(-y)+(-x))-((-y)-8.2)*((-x)*(-y)-(-y))+6.8*((y/6.2)+(-3.7)+(x*x-7.6+(-5.3))*x/(-5.0)*(-x)-(-0.6)+(-4.4))+((5.3)/(y-y)-(-y)))
            wr.tt <| (I 987)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 988
        ctx.emit.comment "test988"
        //let z0 = ((-x)*(-y)*x+y+x-0.7)
        //printfn "%d" <| 988
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)*(-y)*x+y+x-0.7)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)*(-q)*p+q+p-0.7)
            z2 <== ((-x)*(-y)*x+y+x-0.7)
            wr.tt <| (I 988)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 989
        ctx.emit.comment "test989"
        //let z0 = 8.1
        //printfn "%d" <| 989
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 990
        ctx.emit.comment "test990"
        //let z0 = ((y*((-6.2)-(-6.8)+y)/y-x+6.5*7.3-(-y)+(y-(-x)+(-4.7)/2.5)+(x))-((-y)+(-x)-(0.0*(-1.1))/(6.3-y))+y)
        //printfn "%d" <| 990
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y*((-6.2)-(-6.8)+y)/y-x+6.5*7.3-(-y)+(y-(-x)+(-4.7)/2.5)+(x))-((-y)+(-x)-(0.0*(-1.1))/(6.3-y))+y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q*((-6.2)-(-6.8)+q)/q-p+6.5*7.3-(-q)+(q-(-p)+(-4.7)/2.5)+(p))-((-q)+(-p)-(0.0*(-1.1))/(6.3-q))+q)
            z2 <== ((y*((-6.2)-(-6.8)+y)/y-x+6.5*7.3-(-y)+(y-(-x)+(-4.7)/2.5)+(x))-((-y)+(-x)-(0.0*(-1.1))/(6.3-y))+y)
            wr.tt <| (I 990)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 991
        ctx.emit.comment "test991"
        //let z0 = ((6.1*(1.8-x*(-x)))/(-x))
        //printfn "%d" <| 991
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((6.1*(1.8-x*(-x)))/(-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((6.1*(1.8-p*(-p)))/(-p))
            z2 <== ((6.1*(1.8-x*(-x)))/(-x))
            wr.tt <| (I 991)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 992
        ctx.emit.comment "test992"
        //let z0 = 4.2
        //printfn "%d" <| 992
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 993
        ctx.emit.comment "test993"
        //let z0 = 2.4
        //printfn "%d" <| 993
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 994
        ctx.emit.comment "test994"
        //let z0 = (((-5.3)*y+(-x)+y)*0.5+x/((y+x)))
        //printfn "%d" <| 994
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-5.3)*y+(-x)+y)*0.5+x/((y+x)))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-5.3)*q+(-p)+q)*0.5+p/((q+p)))
            z2 <== (((-5.3)*y+(-x)+y)*0.5+x/((y+x)))
            wr.tt <| (I 994)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 995
        ctx.emit.comment "test995"
        //let z0 = ((-5.8))
        //printfn "%d" <| 995
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 996
        ctx.emit.comment "test996"
        //let z0 = (-x)
        //printfn "%d" <| 996
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr.tt <| (I 996)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 997
        ctx.emit.comment "test997"
        //let z0 = ((x/(-7.2)+(x-y*(-x)+3.1/8.0)-x))
        //printfn "%d" <| 997
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x/(-7.2)+(x-y*(-x)+3.1/8.0)-x))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p/(-7.2)+(p-q*(-p)+3.1/8.0)-p))
            z2 <== ((x/(-7.2)+(x-y*(-x)+3.1/8.0)-x))
            wr.tt <| (I 997)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 998
        ctx.emit.comment "test998"
        //let z0 = (-x)/5.1+(-y)/(x+(-x)*(-2.2)+5.8+8.5)/0.3-((-x)*5.1)/((-y)-x+3.0-y)
        //printfn "%d" <| 998
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)/5.1+(-y)/(x+(-x)*(-2.2)+5.8+8.5)/0.3-((-x)*5.1)/((-y)-x+3.0-y)).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)/5.1+(-q)/(p+(-p)*(-2.2)+5.8+8.5)/0.3-((-p)*5.1)/((-q)-p+3.0-q)
            z2 <== (-x)/5.1+(-y)/(x+(-x)*(-2.2)+5.8+8.5)/0.3-((-x)*5.1)/((-y)-x+3.0-y)
            wr.tt <| (I 998)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 999
        ctx.emit.comment "test999"
        //let z0 = x/((1.1-5.6+0.1*y)*7.3)/(y)*(-x)*((-8.6)*0.3)+(y-(-x)/(-3.8)+y)/y+(-x)/(-y)*2.2+6.3+(((-x)/y*(-5.8))*(y/(-y)*y)*(-x)+8.7+(-y))
        //printfn "%d" <| 999
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x/((1.1-5.6+0.1*y)*7.3)/(y)*(-x)*((-8.6)*0.3)+(y-(-x)/(-3.8)+y)/y+(-x)/(-y)*2.2+6.3+(((-x)/y*(-5.8))*(y/(-y)*y)*(-x)+8.7+(-y))).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p/((1.1-5.6+0.1*q)*7.3)/(q)*(-p)*((-8.6)*0.3)+(q-(-p)/(-3.8)+q)/q+(-p)/(-q)*2.2+6.3+(((-p)/q*(-5.8))*(q/(-q)*q)*(-p)+8.7+(-q))
            z2 <== x/((1.1-5.6+0.1*y)*7.3)/(y)*(-x)*((-8.6)*0.3)+(y-(-x)/(-3.8)+y)/y+(-x)/(-y)*2.2+6.3+(((-x)/y*(-5.8))*(y/(-y)*y)*(-x)+8.7+(-y))
            wr.tt <| (I 999)++z1++z2++asm.abs(z1-z2)
        //printfn "%d" 1000
        ctx.emit.comment "test1000"
        //let z0 = y
        //printfn "%d" <| 1000
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval()
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr.tt <| (I 1000)++z1++z2++asm.abs(z1-z2)
