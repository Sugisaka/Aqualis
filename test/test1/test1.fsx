//#############################################################################
// Aqualisテスト
// 実行後removeNaNを実行し非数値数式をコメントアウト
let projectname = "test1"
let version = "1.0.0"
//#############################################################################

let outputdir = __SOURCE_DIRECTORY__

#I @"..\..\bin\Debug\net10.0"
#r "Aqualis.dll"

open Aqualis

Compile [Fortran;C99;Python] outputdir projectname ("aaa","aaa") <| fun () ->
    io.fileOutput "result.dat" <| fun wr ->
    ch.dddd <| fun (x,y,z1,z2) ->
        let p = 4.7
        let q = 4.6
        x <== p
        y <== q
        //printfn "%d" 1
        !"test001"
        //let z0 = (-7.0)
        //printfn "%d" <| 1
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 2
        !"test002"
        //let z0 = (-y)
        //printfn "%d" <| 2
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 2; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 3
        !"test003"
        //let z0 = 5.8
        //printfn "%d" <| 3
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 4
        !"test004"
        //let z0 = (-7.3)
        //printfn "%d" <| 4
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 5
        !"test005"
        //let z0 = (((-y)-(-y)+(-3.6)-y*(-x))*((-4.8)-8.5*(-7.3)-5.1)*(-x)-8.0*y/(7.8/(-2.0)))+((-6.0)+((-y)*(-3.3)+1.7)+(-y)-2.0)/y-x/((-5.6))
        //printfn "%d" <| 5
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)-(-y)+(-3.6)-y*(-x))*((-4.8)-8.5*(-7.3)-5.1)*(-x)-8.0*y/(7.8/(-2.0)))+((-6.0)+((-y)*(-3.3)+1.7)+(-y)-2.0)/y-x/((-5.6))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)-(-q)+(-3.6)-q*(-p))*((-4.8)-8.5*(-7.3)-5.1)*(-p)-8.0*q/(7.8/(-2.0)))+((-6.0)+((-q)*(-3.3)+1.7)+(-q)-2.0)/q-p/((-5.6))
            z2 <== (((-y)-(-y)+(-3.6)-y*(-x))*((-4.8)-8.5*(-7.3)-5.1)*(-x)-8.0*y/(7.8/(-2.0)))+((-6.0)+((-y)*(-3.3)+1.7)+(-y)-2.0)/y-x/((-5.6))
            wr [I 5; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 6
        !"test006"
        //let z0 = (3.5/(-8.8)-3.3-5.3/((-2.3)/6.3+(-6.6)-(-6.6)-(-0.5))/x*(-1.1)+(-1.3)*(-y)-8.7/((x)-(x/y)+7.5/x)-(-5.1))
        //printfn "%d" <| 6
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((3.5/(-8.8)-3.3-5.3/((-2.3)/6.3+(-6.6)-(-6.6)-(-0.5))/x*(-1.1)+(-1.3)*(-y)-8.7/((x)-(x/y)+7.5/x)-(-5.1))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (3.5/(-8.8)-3.3-5.3/((-2.3)/6.3+(-6.6)-(-6.6)-(-0.5))/p*(-1.1)+(-1.3)*(-q)-8.7/((p)-(p/q)+7.5/p)-(-5.1))
            z2 <== (3.5/(-8.8)-3.3-5.3/((-2.3)/6.3+(-6.6)-(-6.6)-(-0.5))/x*(-1.1)+(-1.3)*(-y)-8.7/((x)-(x/y)+7.5/x)-(-5.1))
            wr [I 6; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 7
        !"test007"
        //let z0 = ((4.2*(-x))+(((-6.3)*(-8.0)*(-6.5)*(-y))*(y/(-0.6)/y+x*(-y))*(-x))*4.1)
        //printfn "%d" <| 7
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((4.2*(-x))+(((-6.3)*(-8.0)*(-6.5)*(-y))*(y/(-0.6)/y+x*(-y))*(-x))*4.1)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((4.2*(-p))+(((-6.3)*(-8.0)*(-6.5)*(-q))*(q/(-0.6)/q+p*(-q))*(-p))*4.1)
            z2 <== ((4.2*(-x))+(((-6.3)*(-8.0)*(-6.5)*(-y))*(y/(-0.6)/y+x*(-y))*(-x))*4.1)
            wr [I 7; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 8
        !"test008"
        //let z0 = ((-1.7)+((-7.8))*1.0)
        //printfn "%d" <| 8
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 9
        !"test009"
        //let z0 = y
        //printfn "%d" <| 9
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 9; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 10
        !"test010"
        //let z0 = y
        //printfn "%d" <| 10
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 10; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 11
        !"test011"
        //let z0 = ((-0.8)+y+x+(((-8.2)-(-x)*(-2.2)+x)/(-x))-(0.1/x/(-6.2)*x)*(y)+7.2+(-8.3)-((-1.6)))
        //printfn "%d" <| 11
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.8)+y+x+(((-8.2)-(-x)*(-2.2)+x)/(-x))-(0.1/x/(-6.2)*x)*(y)+7.2+(-8.3)-((-1.6)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.8)+q+p+(((-8.2)-(-p)*(-2.2)+p)/(-p))-(0.1/p/(-6.2)*p)*(q)+7.2+(-8.3)-((-1.6)))
            z2 <== ((-0.8)+y+x+(((-8.2)-(-x)*(-2.2)+x)/(-x))-(0.1/x/(-6.2)*x)*(y)+7.2+(-8.3)-((-1.6)))
            wr [I 11; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 12
        !"test012"
        //let z0 = x
        //printfn "%d" <| 12
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 12; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 13
        !"test013"
        //let z0 = (x/(y-(-x)+x+x)*((-0.2)/((-y)/x+(-y)-0.6)*x+(x)/((-y)/2.6-(-1.8)/(-y)))*0.5/((8.2-8.5-5.4/(-x)))+(((-0.8)-(-5.1)/4.6*(-6.5)-4.4)*(2.3-7.5)+(y)/(-y)))
        //printfn "%d" <| 13
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x/(y-(-x)+x+x)*((-0.2)/((-y)/x+(-y)-0.6)*x+(x)/((-y)/2.6-(-1.8)/(-y)))*0.5/((8.2-8.5-5.4/(-x)))+(((-0.8)-(-5.1)/4.6*(-6.5)-4.4)*(2.3-7.5)+(y)/(-y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p/(q-(-p)+p+p)*((-0.2)/((-q)/p+(-q)-0.6)*p+(p)/((-q)/2.6-(-1.8)/(-q)))*0.5/((8.2-8.5-5.4/(-p)))+(((-0.8)-(-5.1)/4.6*(-6.5)-4.4)*(2.3-7.5)+(q)/(-q)))
            z2 <== (x/(y-(-x)+x+x)*((-0.2)/((-y)/x+(-y)-0.6)*x+(x)/((-y)/2.6-(-1.8)/(-y)))*0.5/((8.2-8.5-5.4/(-x)))+(((-0.8)-(-5.1)/4.6*(-6.5)-4.4)*(2.3-7.5)+(y)/(-y)))
            wr [I 13; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 14
        !"test014"
        //let z0 = ((y+y)+x-y/y/(y+(-2.1)-5.1/(-x))+(((-8.7)-y)/3.6))
        //printfn "%d" <| 14
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y+y)+x-y/y/(y+(-2.1)-5.1/(-x))+(((-8.7)-y)/3.6))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q+q)+p-q/q/(q+(-2.1)-5.1/(-p))+(((-8.7)-q)/3.6))
            z2 <== ((y+y)+x-y/y/(y+(-2.1)-5.1/(-x))+(((-8.7)-y)/3.6))
            wr [I 14; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 15
        !"test015"
        //let z0 = (-3.6)
        //printfn "%d" <| 15
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 16
        !"test016"
        //let z0 = (((x)-(-y)-(-7.5)/((-2.8)+4.6+6.6*(-y)*(-y))))
        //printfn "%d" <| 16
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((x)-(-y)-(-7.5)/((-2.8)+4.6+6.6*(-y)*(-y))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((p)-(-q)-(-7.5)/((-2.8)+4.6+6.6*(-q)*(-q))))
            z2 <== (((x)-(-y)-(-7.5)/((-2.8)+4.6+6.6*(-y)*(-y))))
            wr [I 16; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 17
        !"test017"
        //let z0 = x
        //printfn "%d" <| 17
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 17; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 18
        !"test018"
        //let z0 = (-0.0)/x/(-y)/((-1.7)+(-y)+((-4.0)+(-x)-4.7)*(6.8)-((-x)/(-6.3)*(-4.8)))
        //printfn "%d" <| 18
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-0.0)/x/(-y)/((-1.7)+(-y)+((-4.0)+(-x)-4.7)*(6.8)-((-x)/(-6.3)*(-4.8)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-0.0)/p/(-q)/((-1.7)+(-q)+((-4.0)+(-p)-4.7)*(6.8)-((-p)/(-6.3)*(-4.8)))
            z2 <== (-0.0)/x/(-y)/((-1.7)+(-y)+((-4.0)+(-x)-4.7)*(6.8)-((-x)/(-6.3)*(-4.8)))
            wr [I 18; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 19
        !"test019"
        //let z0 = (-7.8)
        //printfn "%d" <| 19
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 20
        !"test020"
        //let z0 = ((-y)/(((-8.3)+(-y)*y)+(x)-(y+(-y)-(-2.4)/(-0.7))-y/5.8/y-(-2.3)-(-2.1))-(-7.2)+(-x)/(-8.7))
        //printfn "%d" <| 20
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)/(((-8.3)+(-y)*y)+(x)-(y+(-y)-(-2.4)/(-0.7))-y/5.8/y-(-2.3)-(-2.1))-(-7.2)+(-x)/(-8.7))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)/(((-8.3)+(-q)*q)+(p)-(q+(-q)-(-2.4)/(-0.7))-q/5.8/q-(-2.3)-(-2.1))-(-7.2)+(-p)/(-8.7))
            z2 <== ((-y)/(((-8.3)+(-y)*y)+(x)-(y+(-y)-(-2.4)/(-0.7))-y/5.8/y-(-2.3)-(-2.1))-(-7.2)+(-x)/(-8.7))
            wr [I 20; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 21
        !"test021"
        //let z0 = y+(2.3-y-(-y)+(-5.6)-(-x))+(7.3*((-x)*x*(-5.7)))
        //printfn "%d" <| 21
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y+(2.3-y-(-y)+(-5.6)-(-x))+(7.3*((-x)*x*(-5.7)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q+(2.3-q-(-q)+(-5.6)-(-p))+(7.3*((-p)*p*(-5.7)))
            z2 <== y+(2.3-y-(-y)+(-5.6)-(-x))+(7.3*((-x)*x*(-5.7)))
            wr [I 21; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 22
        !"test022"
        //let z0 = y
        //printfn "%d" <| 22
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 22; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 23
        !"test023"
        //let z0 = (-x)
        //printfn "%d" <| 23
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 23; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 24
        !"test024"
        //let z0 = x
        //printfn "%d" <| 24
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 24; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 25
        !"test025"
        //let z0 = y
        //printfn "%d" <| 25
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 25; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 26
        !"test026"
        //let z0 = (2.5-((0.4+(-x)+5.8)/3.7*1.7/5.0+3.5-(-x)+6.5)-(((-5.2)/y*y-y)+((-6.1)/y/0.2*(-x)*(-x)))+y-((x/8.1+y)+(-3.7)/4.5*0.3+((-x))))
        //printfn "%d" <| 26
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((2.5-((0.4+(-x)+5.8)/3.7*1.7/5.0+3.5-(-x)+6.5)-(((-5.2)/y*y-y)+((-6.1)/y/0.2*(-x)*(-x)))+y-((x/8.1+y)+(-3.7)/4.5*0.3+((-x))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (2.5-((0.4+(-p)+5.8)/3.7*1.7/5.0+3.5-(-p)+6.5)-(((-5.2)/q*q-q)+((-6.1)/q/0.2*(-p)*(-p)))+q-((p/8.1+q)+(-3.7)/4.5*0.3+((-p))))
            z2 <== (2.5-((0.4+(-x)+5.8)/3.7*1.7/5.0+3.5-(-x)+6.5)-(((-5.2)/y*y-y)+((-6.1)/y/0.2*(-x)*(-x)))+y-((x/8.1+y)+(-3.7)/4.5*0.3+((-x))))
            wr [I 26; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 27
        !"test027"
        //let z0 = (-x)
        //printfn "%d" <| 27
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 27; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 28
        !"test028"
        //let z0 = y
        //printfn "%d" <| 28
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 28; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 29
        !"test029"
        //let z0 = (-5.2)
        //printfn "%d" <| 29
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 30
        !"test030"
        //let z0 = 8.2
        //printfn "%d" <| 30
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 31
        !"test031"
        //let z0 = (-x)
        //printfn "%d" <| 31
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 31; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 32
        !"test032"
        //let z0 = (((-x)+5.2*3.0/7.0+4.4)/y-(-y)-(x+y+(-7.7)*(-8.0))*0.6-7.4*1.5)+((-0.3)+(-x))*(-y)
        //printfn "%d" <| 32
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)+5.2*3.0/7.0+4.4)/y-(-y)-(x+y+(-7.7)*(-8.0))*0.6-7.4*1.5)+((-0.3)+(-x))*(-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)+5.2*3.0/7.0+4.4)/q-(-q)-(p+q+(-7.7)*(-8.0))*0.6-7.4*1.5)+((-0.3)+(-p))*(-q)
            z2 <== (((-x)+5.2*3.0/7.0+4.4)/y-(-y)-(x+y+(-7.7)*(-8.0))*0.6-7.4*1.5)+((-0.3)+(-x))*(-y)
            wr [I 32; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 33
        !"test033"
        //let z0 = 4.8
        //printfn "%d" <| 33
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 34
        !"test034"
        //let z0 = (-8.2)
        //printfn "%d" <| 34
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 35
        !"test035"
        //let z0 = 2.2
        //printfn "%d" <| 35
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 36
        !"test036"
        //let z0 = x
        //printfn "%d" <| 36
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 36; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 37
        !"test037"
        //let z0 = ((-x)*(5.0+(-y)*((-0.7)*y))-x-(1.6))
        //printfn "%d" <| 37
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)*(5.0+(-y)*((-0.7)*y))-x-(1.6))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)*(5.0+(-q)*((-0.7)*q))-p-(1.6))
            z2 <== ((-x)*(5.0+(-y)*((-0.7)*y))-x-(1.6))
            wr [I 37; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 38
        !"test038"
        //let z0 = (-0.3)
        //printfn "%d" <| 38
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 39
        !"test039"
        //let z0 = y
        //printfn "%d" <| 39
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 39; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 40
        !"test040"
        //let z0 = ((y/x-(x+y-y/x*(-3.0))-(2.6+1.3-x+y/(-y)))-(-5.5)/((-y)+x/(-3.3)-0.1))
        //printfn "%d" <| 40
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y/x-(x+y-y/x*(-3.0))-(2.6+1.3-x+y/(-y)))-(-5.5)/((-y)+x/(-3.3)-0.1))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q/p-(p+q-q/p*(-3.0))-(2.6+1.3-p+q/(-q)))-(-5.5)/((-q)+p/(-3.3)-0.1))
            z2 <== ((y/x-(x+y-y/x*(-3.0))-(2.6+1.3-x+y/(-y)))-(-5.5)/((-y)+x/(-3.3)-0.1))
            wr [I 40; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 41
        !"test041"
        //let z0 = x-(-y)-((-0.3))-(-y)
        //printfn "%d" <| 41
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x-(-y)-((-0.3))-(-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p-(-q)-((-0.3))-(-q)
            z2 <== x-(-y)-((-0.3))-(-y)
            wr [I 41; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 42
        !"test042"
        //let z0 = ((3.3+(-2.1)*4.0+1.5+((-x)*(-4.6))/(-y))/1.4)
        //printfn "%d" <| 42
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((3.3+(-2.1)*4.0+1.5+((-x)*(-4.6))/(-y))/1.4)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((3.3+(-2.1)*4.0+1.5+((-p)*(-4.6))/(-q))/1.4)
            z2 <== ((3.3+(-2.1)*4.0+1.5+((-x)*(-4.6))/(-y))/1.4)
            wr [I 42; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 43
        !"test043"
        //let z0 = 3.1
        //printfn "%d" <| 43
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 44
        !"test044"
        //let z0 = (-y)
        //printfn "%d" <| 44
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 44; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 45
        !"test045"
        //let z0 = x
        //printfn "%d" <| 45
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 45; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 46
        !"test046"
        //let z0 = 0.4-1.2/y
        //printfn "%d" <| 46
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (0.4-1.2/y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 0.4-1.2/q
            z2 <== 0.4-1.2/y
            wr [I 46; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 47
        !"test047"
        //let z0 = x
        //printfn "%d" <| 47
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 47; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 48
        !"test048"
        //let z0 = (y)
        //printfn "%d" <| 48
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q)
            z2 <== (y)
            wr [I 48; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 49
        !"test049"
        //let z0 = ((-2.4))
        //printfn "%d" <| 49
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 50
        !"test050"
        //let z0 = (-0.5)
        //printfn "%d" <| 50
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 51
        !"test051"
        //let z0 = (-x)
        //printfn "%d" <| 51
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 51; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 52
        !"test052"
        //let z0 = (8.0*y-(7.8-((-5.7)*(-y)*(-y)))-(((-y)+y*(-y))/(x*(-4.0))/4.6*(x*(-x)-1.0/(-y)))-(y+6.1+(7.1+(-2.2)/7.4+(-y)+(-x))/(-x)*(-1.7)))
        //printfn "%d" <| 52
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((8.0*y-(7.8-((-5.7)*(-y)*(-y)))-(((-y)+y*(-y))/(x*(-4.0))/4.6*(x*(-x)-1.0/(-y)))-(y+6.1+(7.1+(-2.2)/7.4+(-y)+(-x))/(-x)*(-1.7)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (8.0*q-(7.8-((-5.7)*(-q)*(-q)))-(((-q)+q*(-q))/(p*(-4.0))/4.6*(p*(-p)-1.0/(-q)))-(q+6.1+(7.1+(-2.2)/7.4+(-q)+(-p))/(-p)*(-1.7)))
            z2 <== (8.0*y-(7.8-((-5.7)*(-y)*(-y)))-(((-y)+y*(-y))/(x*(-4.0))/4.6*(x*(-x)-1.0/(-y)))-(y+6.1+(7.1+(-2.2)/7.4+(-y)+(-x))/(-x)*(-1.7)))
            wr [I 52; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 53
        !"test053"
        //let z0 = 7.6
        //printfn "%d" <| 53
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 54
        !"test054"
        //let z0 = (((y-(-1.4))+5.4/(-4.0)/((-x)+x))*1.5)
        //printfn "%d" <| 54
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((y-(-1.4))+5.4/(-4.0)/((-x)+x))*1.5)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((q-(-1.4))+5.4/(-4.0)/((-p)+p))*1.5)
            z2 <== (((y-(-1.4))+5.4/(-4.0)/((-x)+x))*1.5)
            wr [I 54; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 55
        !"test055"
        //let z0 = y
        //printfn "%d" <| 55
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 55; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 56
        !"test056"
        //let z0 = (y*(3.1/6.2*2.3+(-x)/(-2.2))+(3.6/(-7.4)*y+(-3.1))+(x/2.0+(-0.1)/(-y))/y)*((y-(-2.5))+((-x)-y)/(-3.0)-(-2.2)/(-4.7))*(6.2/x*(3.2-x*(-6.3)*(-2.3))-((-x)*(-0.1)*(-3.0)))-((8.3+(-y)-2.0/(-5.8)+5.5)/y)
        //printfn "%d" <| 56
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y*(3.1/6.2*2.3+(-x)/(-2.2))+(3.6/(-7.4)*y+(-3.1))+(x/2.0+(-0.1)/(-y))/y)*((y-(-2.5))+((-x)-y)/(-3.0)-(-2.2)/(-4.7))*(6.2/x*(3.2-x*(-6.3)*(-2.3))-((-x)*(-0.1)*(-3.0)))-((8.3+(-y)-2.0/(-5.8)+5.5)/y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q*(3.1/6.2*2.3+(-p)/(-2.2))+(3.6/(-7.4)*q+(-3.1))+(p/2.0+(-0.1)/(-q))/q)*((q-(-2.5))+((-p)-q)/(-3.0)-(-2.2)/(-4.7))*(6.2/p*(3.2-p*(-6.3)*(-2.3))-((-p)*(-0.1)*(-3.0)))-((8.3+(-q)-2.0/(-5.8)+5.5)/q)
            z2 <== (y*(3.1/6.2*2.3+(-x)/(-2.2))+(3.6/(-7.4)*y+(-3.1))+(x/2.0+(-0.1)/(-y))/y)*((y-(-2.5))+((-x)-y)/(-3.0)-(-2.2)/(-4.7))*(6.2/x*(3.2-x*(-6.3)*(-2.3))-((-x)*(-0.1)*(-3.0)))-((8.3+(-y)-2.0/(-5.8)+5.5)/y)
            wr [I 56; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 57
        !"test057"
        //let z0 = x
        //printfn "%d" <| 57
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 57; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 58
        !"test058"
        //let z0 = ((((-3.0)/y)-(y/(-7.3)*x))*(-x)/((-y)-((-5.3)+x/(-1.8)+5.0+4.5)/((-y)+y/1.1/5.4)*((-x)+(-0.4)+y/3.3+(-3.0)))/1.5)
        //printfn "%d" <| 58
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-3.0)/y)-(y/(-7.3)*x))*(-x)/((-y)-((-5.3)+x/(-1.8)+5.0+4.5)/((-y)+y/1.1/5.4)*((-x)+(-0.4)+y/3.3+(-3.0)))/1.5)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-3.0)/q)-(q/(-7.3)*p))*(-p)/((-q)-((-5.3)+p/(-1.8)+5.0+4.5)/((-q)+q/1.1/5.4)*((-p)+(-0.4)+q/3.3+(-3.0)))/1.5)
            z2 <== ((((-3.0)/y)-(y/(-7.3)*x))*(-x)/((-y)-((-5.3)+x/(-1.8)+5.0+4.5)/((-y)+y/1.1/5.4)*((-x)+(-0.4)+y/3.3+(-3.0)))/1.5)
            wr [I 58; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 59
        !"test059"
        //let z0 = x
        //printfn "%d" <| 59
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 59; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 60
        !"test060"
        //let z0 = 1.8
        //printfn "%d" <| 60
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 61
        !"test061"
        //let z0 = ((((-1.2)-x)/6.1-x)*((-y)*y/(-3.1)*(-y)+(-2.0))/((-2.0)*x-7.3*(-y)/(-x)*(7.6+(-6.4)/(-x)*y+(-y))/(-x)))
        //printfn "%d" <| 61
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-1.2)-x)/6.1-x)*((-y)*y/(-3.1)*(-y)+(-2.0))/((-2.0)*x-7.3*(-y)/(-x)*(7.6+(-6.4)/(-x)*y+(-y))/(-x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-1.2)-p)/6.1-p)*((-q)*q/(-3.1)*(-q)+(-2.0))/((-2.0)*p-7.3*(-q)/(-p)*(7.6+(-6.4)/(-p)*q+(-q))/(-p)))
            z2 <== ((((-1.2)-x)/6.1-x)*((-y)*y/(-3.1)*(-y)+(-2.0))/((-2.0)*x-7.3*(-y)/(-x)*(7.6+(-6.4)/(-x)*y+(-y))/(-x)))
            wr [I 61; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 62
        !"test062"
        //let z0 = ((-x)+(x-3.6+1.1/y*(-y))-(x+(-y)/(-y))*x)
        //printfn "%d" <| 62
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+(x-3.6+1.1/y*(-y))-(x+(-y)/(-y))*x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+(p-3.6+1.1/q*(-q))-(p+(-q)/(-q))*p)
            z2 <== ((-x)+(x-3.6+1.1/y*(-y))-(x+(-y)/(-y))*x)
            wr [I 62; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 63
        !"test063"
        //let z0 = ((-y))
        //printfn "%d" <| 63
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q))
            z2 <== ((-y))
            wr [I 63; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 64
        !"test064"
        //let z0 = ((y-(5.8+x-x))*(y/(1.2/(-0.5)-(-8.5)*x)-(-x)/(7.5*(-5.4)*2.2))/6.8/(((-1.7)))/(5.6))
        //printfn "%d" <| 64
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y-(5.8+x-x))*(y/(1.2/(-0.5)-(-8.5)*x)-(-x)/(7.5*(-5.4)*2.2))/6.8/(((-1.7)))/(5.6))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q-(5.8+p-p))*(q/(1.2/(-0.5)-(-8.5)*p)-(-p)/(7.5*(-5.4)*2.2))/6.8/(((-1.7)))/(5.6))
            z2 <== ((y-(5.8+x-x))*(y/(1.2/(-0.5)-(-8.5)*x)-(-x)/(7.5*(-5.4)*2.2))/6.8/(((-1.7)))/(5.6))
            wr [I 64; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 65
        !"test065"
        //let z0 = (-3.7)
        //printfn "%d" <| 65
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 66
        !"test066"
        //let z0 = (-2.7)
        //printfn "%d" <| 66
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 67
        !"test067"
        //let z0 = (((7.0*(-y)*4.0)*(-3.5)/(-y))*(-2.3)/((y*y)-((-x)/x+(-4.5)-(-y))-((-y)/2.4/(-7.4))+6.1)+(-8.4))
        //printfn "%d" <| 67
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((7.0*(-y)*4.0)*(-3.5)/(-y))*(-2.3)/((y*y)-((-x)/x+(-4.5)-(-y))-((-y)/2.4/(-7.4))+6.1)+(-8.4))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((7.0*(-q)*4.0)*(-3.5)/(-q))*(-2.3)/((q*q)-((-p)/p+(-4.5)-(-q))-((-q)/2.4/(-7.4))+6.1)+(-8.4))
            z2 <== (((7.0*(-y)*4.0)*(-3.5)/(-y))*(-2.3)/((y*y)-((-x)/x+(-4.5)-(-y))-((-y)/2.4/(-7.4))+6.1)+(-8.4))
            wr [I 67; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 68
        !"test068"
        //let z0 = x
        //printfn "%d" <| 68
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 68; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 69
        !"test069"
        //let z0 = x
        //printfn "%d" <| 69
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 69; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 70
        !"test070"
        //let z0 = 4.4
        //printfn "%d" <| 70
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 71
        !"test071"
        //let z0 = (((4.5-(-x)/(-4.0)-1.0-(-x))))
        //printfn "%d" <| 71
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((4.5-(-x)/(-4.0)-1.0-(-x))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((4.5-(-p)/(-4.0)-1.0-(-p))))
            z2 <== (((4.5-(-x)/(-4.0)-1.0-(-x))))
            wr [I 71; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 72
        !"test072"
        //let z0 = ((x-(-7.3)*(0.0/(-0.5)-6.7*x))-(y/(-1.6)+7.4+0.2/((-5.1)*(-8.4)/(-6.2))+(-x)*((-x)/(-0.6)))-(y*7.8+(-7.0)-(-y)+(-y)+(-7.5)*(-7.1)-(-3.3)))
        //printfn "%d" <| 72
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x-(-7.3)*(0.0/(-0.5)-6.7*x))-(y/(-1.6)+7.4+0.2/((-5.1)*(-8.4)/(-6.2))+(-x)*((-x)/(-0.6)))-(y*7.8+(-7.0)-(-y)+(-y)+(-7.5)*(-7.1)-(-3.3)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p-(-7.3)*(0.0/(-0.5)-6.7*p))-(q/(-1.6)+7.4+0.2/((-5.1)*(-8.4)/(-6.2))+(-p)*((-p)/(-0.6)))-(q*7.8+(-7.0)-(-q)+(-q)+(-7.5)*(-7.1)-(-3.3)))
            z2 <== ((x-(-7.3)*(0.0/(-0.5)-6.7*x))-(y/(-1.6)+7.4+0.2/((-5.1)*(-8.4)/(-6.2))+(-x)*((-x)/(-0.6)))-(y*7.8+(-7.0)-(-y)+(-y)+(-7.5)*(-7.1)-(-3.3)))
            wr [I 72; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 73
        !"test073"
        //let z0 = (((-6.6)-(-x)+x-(-x)*(-5.2)/((-0.4)*(-4.7)/(-1.8)))*x-7.4/0.7/x)
        //printfn "%d" <| 73
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-6.6)-(-x)+x-(-x)*(-5.2)/((-0.4)*(-4.7)/(-1.8)))*x-7.4/0.7/x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-6.6)-(-p)+p-(-p)*(-5.2)/((-0.4)*(-4.7)/(-1.8)))*p-7.4/0.7/p)
            z2 <== (((-6.6)-(-x)+x-(-x)*(-5.2)/((-0.4)*(-4.7)/(-1.8)))*x-7.4/0.7/x)
            wr [I 73; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 74
        !"test074"
        //let z0 = (-3.6)
        //printfn "%d" <| 74
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 75
        !"test075"
        //let z0 = ((x+(-7.4)*4.3/(-y))*y/(-7.7)/(-4.4)*x-y/(-7.1)+((-x)-6.7)+((-5.6)/(-6.7)/((-6.5)+(-8.7))))
        //printfn "%d" <| 75
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x+(-7.4)*4.3/(-y))*y/(-7.7)/(-4.4)*x-y/(-7.1)+((-x)-6.7)+((-5.6)/(-6.7)/((-6.5)+(-8.7))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p+(-7.4)*4.3/(-q))*q/(-7.7)/(-4.4)*p-q/(-7.1)+((-p)-6.7)+((-5.6)/(-6.7)/((-6.5)+(-8.7))))
            z2 <== ((x+(-7.4)*4.3/(-y))*y/(-7.7)/(-4.4)*x-y/(-7.1)+((-x)-6.7)+((-5.6)/(-6.7)/((-6.5)+(-8.7))))
            wr [I 75; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 76
        !"test076"
        //let z0 = ((-y)+(-x)+((y)-(-y)-((-6.0)/6.8/(-x)/x)*x-(-x))+(-x)*x)
        //printfn "%d" <| 76
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)+(-x)+((y)-(-y)-((-6.0)/6.8/(-x)/x)*x-(-x))+(-x)*x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)+(-p)+((q)-(-q)-((-6.0)/6.8/(-p)/p)*p-(-p))+(-p)*p)
            z2 <== ((-y)+(-x)+((y)-(-y)-((-6.0)/6.8/(-x)/x)*x-(-x))+(-x)*x)
            wr [I 76; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 77
        !"test077"
        //let z0 = 1.3
        //printfn "%d" <| 77
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 78
        !"test078"
        //let z0 = (((-5.5)))
        //printfn "%d" <| 78
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 79
        !"test079"
        //let z0 = 8.3
        //printfn "%d" <| 79
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 80
        !"test080"
        //let z0 = (((-1.8)*((-y)*y/(-8.8)+7.3+(-y))+y-(-6.8)+(1.6/(-x)*(-x)/0.8/x))/((-8.0)+3.2-x*y+8.6/8.1))
        //printfn "%d" <| 80
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-1.8)*((-y)*y/(-8.8)+7.3+(-y))+y-(-6.8)+(1.6/(-x)*(-x)/0.8/x))/((-8.0)+3.2-x*y+8.6/8.1))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-1.8)*((-q)*q/(-8.8)+7.3+(-q))+q-(-6.8)+(1.6/(-p)*(-p)/0.8/p))/((-8.0)+3.2-p*q+8.6/8.1))
            z2 <== (((-1.8)*((-y)*y/(-8.8)+7.3+(-y))+y-(-6.8)+(1.6/(-x)*(-x)/0.8/x))/((-8.0)+3.2-x*y+8.6/8.1))
            wr [I 80; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 81
        !"test081"
        //let z0 = 2.6
        //printfn "%d" <| 81
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 82
        !"test082"
        //let z0 = (0.2+((-y)/((-x))-(y))/(-y)-((x*y*(-x)*(-4.6)-(-7.3))-(y)))
        //printfn "%d" <| 82
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((0.2+((-y)/((-x))-(y))/(-y)-((x*y*(-x)*(-4.6)-(-7.3))-(y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (0.2+((-q)/((-p))-(q))/(-q)-((p*q*(-p)*(-4.6)-(-7.3))-(q)))
            z2 <== (0.2+((-y)/((-x))-(y))/(-y)-((x*y*(-x)*(-4.6)-(-7.3))-(y)))
            wr [I 82; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 83
        !"test083"
        //let z0 = 6.1
        //printfn "%d" <| 83
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 84
        !"test084"
        //let z0 = x
        //printfn "%d" <| 84
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 84; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 85
        !"test085"
        //let z0 = (x)
        //printfn "%d" <| 85
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p)
            z2 <== (x)
            wr [I 85; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 86
        !"test086"
        //let z0 = ((y-(-5.6)/(-2.6))-8.6)
        //printfn "%d" <| 86
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y-(-5.6)/(-2.6))-8.6)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q-(-5.6)/(-2.6))-8.6)
            z2 <== ((y-(-5.6)/(-2.6))-8.6)
            wr [I 86; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 87
        !"test087"
        //let z0 = (x*y+(-x)*(-6.4)/(1.5/(-0.6)/(x)-(-y)*((-6.0)-3.5*5.5*(-x))))
        //printfn "%d" <| 87
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x*y+(-x)*(-6.4)/(1.5/(-0.6)/(x)-(-y)*((-6.0)-3.5*5.5*(-x))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p*q+(-p)*(-6.4)/(1.5/(-0.6)/(p)-(-q)*((-6.0)-3.5*5.5*(-p))))
            z2 <== (x*y+(-x)*(-6.4)/(1.5/(-0.6)/(x)-(-y)*((-6.0)-3.5*5.5*(-x))))
            wr [I 87; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 88
        !"test088"
        //let z0 = (x-(-y))
        //printfn "%d" <| 88
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x-(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p-(-q))
            z2 <== (x-(-y))
            wr [I 88; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 89
        !"test089"
        //let z0 = ((3.6-y+(-x)/(-x))/(((-y)-1.7)/(-4.2)/((-6.5)*(-4.7)+(-x))-(5.4*0.4+y*0.4))*(-y)-((-0.4)-(y+y/(-4.3))/((-y)/x*(-x))-(y+x+(-0.3)+7.7-1.0)))
        //printfn "%d" <| 89
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((3.6-y+(-x)/(-x))/(((-y)-1.7)/(-4.2)/((-6.5)*(-4.7)+(-x))-(5.4*0.4+y*0.4))*(-y)-((-0.4)-(y+y/(-4.3))/((-y)/x*(-x))-(y+x+(-0.3)+7.7-1.0)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((3.6-q+(-p)/(-p))/(((-q)-1.7)/(-4.2)/((-6.5)*(-4.7)+(-p))-(5.4*0.4+q*0.4))*(-q)-((-0.4)-(q+q/(-4.3))/((-q)/p*(-p))-(q+p+(-0.3)+7.7-1.0)))
            z2 <== ((3.6-y+(-x)/(-x))/(((-y)-1.7)/(-4.2)/((-6.5)*(-4.7)+(-x))-(5.4*0.4+y*0.4))*(-y)-((-0.4)-(y+y/(-4.3))/((-y)/x*(-x))-(y+x+(-0.3)+7.7-1.0)))
            wr [I 89; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 90
        !"test090"
        //let z0 = y
        //printfn "%d" <| 90
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 90; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 91
        !"test091"
        //let z0 = 8.7
        //printfn "%d" <| 91
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 92
        !"test092"
        //let z0 = (-x)
        //printfn "%d" <| 92
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 92; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 93
        !"test093"
        //let z0 = (((-5.8)*(-1.3)*(7.2))*4.1*(x*(-x)+(-x)*x/5.2))
        //printfn "%d" <| 93
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-5.8)*(-1.3)*(7.2))*4.1*(x*(-x)+(-x)*x/5.2))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-5.8)*(-1.3)*(7.2))*4.1*(p*(-p)+(-p)*p/5.2))
            z2 <== (((-5.8)*(-1.3)*(7.2))*4.1*(x*(-x)+(-x)*x/5.2))
            wr [I 93; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 94
        !"test094"
        //let z0 = (-x)
        //printfn "%d" <| 94
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 94; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 95
        !"test095"
        //let z0 = (-3.0)
        //printfn "%d" <| 95
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 96
        !"test096"
        //let z0 = 8.7
        //printfn "%d" <| 96
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 97
        !"test097"
        //let z0 = (-x)
        //printfn "%d" <| 97
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 97; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 98
        !"test098"
        //let z0 = 0.3
        //printfn "%d" <| 98
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 99
        !"test099"
        //let z0 = 8.8
        //printfn "%d" <| 99
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 100
        !"test100"
        //let z0 = (-x)/(0.6-6.7/y/y)-((-y)*5.3-(-1.1)+3.8/(-8.7))/(-x)-(-1.4)+(-1.4)+2.1*5.3-(x/y/(-y))/((-y)/(-x)+(-5.7)+(-x)-(-0.3))
        //printfn "%d" <| 100
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)/(0.6-6.7/y/y)-((-y)*5.3-(-1.1)+3.8/(-8.7))/(-x)-(-1.4)+(-1.4)+2.1*5.3-(x/y/(-y))/((-y)/(-x)+(-5.7)+(-x)-(-0.3))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)/(0.6-6.7/q/q)-((-q)*5.3-(-1.1)+3.8/(-8.7))/(-p)-(-1.4)+(-1.4)+2.1*5.3-(p/q/(-q))/((-q)/(-p)+(-5.7)+(-p)-(-0.3))
            z2 <== (-x)/(0.6-6.7/y/y)-((-y)*5.3-(-1.1)+3.8/(-8.7))/(-x)-(-1.4)+(-1.4)+2.1*5.3-(x/y/(-y))/((-y)/(-x)+(-5.7)+(-x)-(-0.3))
            wr [I 100; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 101
        !"test101"
        //let z0 = (-y)
        //printfn "%d" <| 101
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 101; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 102
        !"test102"
        //let z0 = (-2.7)
        //printfn "%d" <| 102
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 103
        !"test103"
        //let z0 = (-x)
        //printfn "%d" <| 103
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 103; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 104
        !"test104"
        //let z0 = 4.6
        //printfn "%d" <| 104
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 105
        !"test105"
        //let z0 = y
        //printfn "%d" <| 105
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 105; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 106
        !"test106"
        //let z0 = ((2.0)/5.4-7.0-(-1.5))
        //printfn "%d" <| 106
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 107
        !"test107"
        //let z0 = (-x)
        //printfn "%d" <| 107
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 107; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 108
        !"test108"
        //let z0 = (-x)
        //printfn "%d" <| 108
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 108; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 109
        !"test109"
        //let z0 = 3.8
        //printfn "%d" <| 109
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 110
        !"test110"
        //let z0 = (2.2)
        //printfn "%d" <| 110
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 111
        !"test111"
        //let z0 = x
        //printfn "%d" <| 111
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 111; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 112
        !"test112"
        //let z0 = 7.2
        //printfn "%d" <| 112
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 113
        !"test113"
        //let z0 = ((-x)*(((-6.7)-(-1.3))/(-4.1)*(-8.6)+((-x)*y+y))-y+((-2.5)-(-6.0)))
        //printfn "%d" <| 113
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)*(((-6.7)-(-1.3))/(-4.1)*(-8.6)+((-x)*y+y))-y+((-2.5)-(-6.0)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)*(((-6.7)-(-1.3))/(-4.1)*(-8.6)+((-p)*q+q))-q+((-2.5)-(-6.0)))
            z2 <== ((-x)*(((-6.7)-(-1.3))/(-4.1)*(-8.6)+((-x)*y+y))-y+((-2.5)-(-6.0)))
            wr [I 113; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 114
        !"test114"
        //let z0 = (5.2+((-3.2)+(8.4-(-x)*x-(-x)-x))/0.8*y-x+(((-0.6)*(-x)+3.0/(-4.4)/0.4)+5.4-(-6.4))*x)
        //printfn "%d" <| 114
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.2+((-3.2)+(8.4-(-x)*x-(-x)-x))/0.8*y-x+(((-0.6)*(-x)+3.0/(-4.4)/0.4)+5.4-(-6.4))*x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.2+((-3.2)+(8.4-(-p)*p-(-p)-p))/0.8*q-p+(((-0.6)*(-p)+3.0/(-4.4)/0.4)+5.4-(-6.4))*p)
            z2 <== (5.2+((-3.2)+(8.4-(-x)*x-(-x)-x))/0.8*y-x+(((-0.6)*(-x)+3.0/(-4.4)/0.4)+5.4-(-6.4))*x)
            wr [I 114; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 115
        !"test115"
        //let z0 = (x+(-y))
        //printfn "%d" <| 115
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x+(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p+(-q))
            z2 <== (x+(-y))
            wr [I 115; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 116
        !"test116"
        //let z0 = (-y)
        //printfn "%d" <| 116
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 116; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 117
        !"test117"
        //let z0 = y
        //printfn "%d" <| 117
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 117; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 118
        !"test118"
        //let z0 = (1.4-7.4+x+y*y-((-3.1)*(-y)*y/(-4.2))/7.1*(-x)+(-x)+y)
        //printfn "%d" <| 118
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((1.4-7.4+x+y*y-((-3.1)*(-y)*y/(-4.2))/7.1*(-x)+(-x)+y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (1.4-7.4+p+q*q-((-3.1)*(-q)*q/(-4.2))/7.1*(-p)+(-p)+q)
            z2 <== (1.4-7.4+x+y*y-((-3.1)*(-y)*y/(-4.2))/7.1*(-x)+(-x)+y)
            wr [I 118; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 119
        !"test119"
        //let z0 = ((((-7.5)/(-6.8)-(-x))/(2.1/5.1-(-x))*(-3.1)+(-y)*(-3.6)+x/(-x))+((1.5-(-1.8)+(-x)*8.5+7.2)*((-x)+8.4*(-0.0)-y)+(-x)/(-y)+x/y/(-y)/(6.7/x+x+(-x)/(-3.3))+x)-((-7.2)/(x-x+(-7.3)))/(x+(-x)/(-y))-((-y)/(-6.7)/(-7.8)+x)+(-x))
        //printfn "%d" <| 119
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-7.5)/(-6.8)-(-x))/(2.1/5.1-(-x))*(-3.1)+(-y)*(-3.6)+x/(-x))+((1.5-(-1.8)+(-x)*8.5+7.2)*((-x)+8.4*(-0.0)-y)+(-x)/(-y)+x/y/(-y)/(6.7/x+x+(-x)/(-3.3))+x)-((-7.2)/(x-x+(-7.3)))/(x+(-x)/(-y))-((-y)/(-6.7)/(-7.8)+x)+(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-7.5)/(-6.8)-(-p))/(2.1/5.1-(-p))*(-3.1)+(-q)*(-3.6)+p/(-p))+((1.5-(-1.8)+(-p)*8.5+7.2)*((-p)+8.4*(-0.0)-q)+(-p)/(-q)+p/q/(-q)/(6.7/p+p+(-p)/(-3.3))+p)-((-7.2)/(p-p+(-7.3)))/(p+(-p)/(-q))-((-q)/(-6.7)/(-7.8)+p)+(-p))
            z2 <== ((((-7.5)/(-6.8)-(-x))/(2.1/5.1-(-x))*(-3.1)+(-y)*(-3.6)+x/(-x))+((1.5-(-1.8)+(-x)*8.5+7.2)*((-x)+8.4*(-0.0)-y)+(-x)/(-y)+x/y/(-y)/(6.7/x+x+(-x)/(-3.3))+x)-((-7.2)/(x-x+(-7.3)))/(x+(-x)/(-y))-((-y)/(-6.7)/(-7.8)+x)+(-x))
            wr [I 119; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 120
        !"test120"
        //let z0 = (-7.3)
        //printfn "%d" <| 120
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 121
        !"test121"
        //let z0 = (-y)
        //printfn "%d" <| 121
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 121; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 122
        !"test122"
        //let z0 = (-2.0)
        //printfn "%d" <| 122
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 123
        !"test123"
        //let z0 = ((((-2.8)*x+4.8)+x/y-(y/y-x-(-4.7))/((-8.0)))+(-y)-5.8-(y)-x*((-y)/y)-((-5.3))+((5.4)-(x-(-x)*(-7.2)+(-1.6)*3.3)*((-x)+(-x)/(-8.8)-(-x)-(-x))-(8.7))/((2.8-y-x)-1.2/(-0.0)+0.2*(-3.6)/5.1/0.5))
        //printfn "%d" <| 123
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-2.8)*x+4.8)+x/y-(y/y-x-(-4.7))/((-8.0)))+(-y)-5.8-(y)-x*((-y)/y)-((-5.3))+((5.4)-(x-(-x)*(-7.2)+(-1.6)*3.3)*((-x)+(-x)/(-8.8)-(-x)-(-x))-(8.7))/((2.8-y-x)-1.2/(-0.0)+0.2*(-3.6)/5.1/0.5))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-2.8)*p+4.8)+p/q-(q/q-p-(-4.7))/((-8.0)))+(-q)-5.8-(q)-p*((-q)/q)-((-5.3))+((5.4)-(p-(-p)*(-7.2)+(-1.6)*3.3)*((-p)+(-p)/(-8.8)-(-p)-(-p))-(8.7))/((2.8-q-p)-1.2/(-0.0)+0.2*(-3.6)/5.1/0.5))
            z2 <== ((((-2.8)*x+4.8)+x/y-(y/y-x-(-4.7))/((-8.0)))+(-y)-5.8-(y)-x*((-y)/y)-((-5.3))+((5.4)-(x-(-x)*(-7.2)+(-1.6)*3.3)*((-x)+(-x)/(-8.8)-(-x)-(-x))-(8.7))/((2.8-y-x)-1.2/(-0.0)+0.2*(-3.6)/5.1/0.5))
            wr [I 123; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 124
        !"test124"
        //let z0 = y
        //printfn "%d" <| 124
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 124; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 125
        !"test125"
        //let z0 = ((-x))
        //printfn "%d" <| 125
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p))
            z2 <== ((-x))
            wr [I 125; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 126
        !"test126"
        //let z0 = (-0.3)+8.4
        //printfn "%d" <| 126
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 127
        !"test127"
        //let z0 = (x/y*(-x)*(y+x)/(-5.5))
        //printfn "%d" <| 127
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x/y*(-x)*(y+x)/(-5.5))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p/q*(-p)*(q+p)/(-5.5))
            z2 <== (x/y*(-x)*(y+x)/(-5.5))
            wr [I 127; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 128
        !"test128"
        //let z0 = (-5.6)
        //printfn "%d" <| 128
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 129
        !"test129"
        //let z0 = y
        //printfn "%d" <| 129
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 129; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 130
        !"test130"
        //let z0 = (-y)
        //printfn "%d" <| 130
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 130; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 131
        !"test131"
        //let z0 = x
        //printfn "%d" <| 131
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 131; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 132
        !"test132"
        //let z0 = 7.7
        //printfn "%d" <| 132
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 133
        !"test133"
        //let z0 = 1.8
        //printfn "%d" <| 133
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 134
        !"test134"
        //let z0 = (-x)+(x/(-6.7)*y*(-5.4)*(-7.5))/(y*(-y))-(y+1.8/(-x)/4.7)+2.8
        //printfn "%d" <| 134
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)+(x/(-6.7)*y*(-5.4)*(-7.5))/(y*(-y))-(y+1.8/(-x)/4.7)+2.8).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)+(p/(-6.7)*q*(-5.4)*(-7.5))/(q*(-q))-(q+1.8/(-p)/4.7)+2.8
            z2 <== (-x)+(x/(-6.7)*y*(-5.4)*(-7.5))/(y*(-y))-(y+1.8/(-x)/4.7)+2.8
            wr [I 134; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 135
        !"test135"
        //let z0 = ((-0.3)+(5.3/(x/(-y)+(-3.3)/y+(-y)))+((-y)-(-x)/(-8.3)/2.2*((-2.5)*x*(-x)*0.8-y)))
        //printfn "%d" <| 135
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.3)+(5.3/(x/(-y)+(-3.3)/y+(-y)))+((-y)-(-x)/(-8.3)/2.2*((-2.5)*x*(-x)*0.8-y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.3)+(5.3/(p/(-q)+(-3.3)/q+(-q)))+((-q)-(-p)/(-8.3)/2.2*((-2.5)*p*(-p)*0.8-q)))
            z2 <== ((-0.3)+(5.3/(x/(-y)+(-3.3)/y+(-y)))+((-y)-(-x)/(-8.3)/2.2*((-2.5)*x*(-x)*0.8-y)))
            wr [I 135; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 136
        !"test136"
        //let z0 = (-2.5)
        //printfn "%d" <| 136
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 137
        !"test137"
        //let z0 = (y-(-6.1)-(x/(-5.2)/x/6.0*(x+(-8.1)-(-1.2)/2.1+0.2)*(8.0)+(-y)))
        //printfn "%d" <| 137
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y-(-6.1)-(x/(-5.2)/x/6.0*(x+(-8.1)-(-1.2)/2.1+0.2)*(8.0)+(-y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q-(-6.1)-(p/(-5.2)/p/6.0*(p+(-8.1)-(-1.2)/2.1+0.2)*(8.0)+(-q)))
            z2 <== (y-(-6.1)-(x/(-5.2)/x/6.0*(x+(-8.1)-(-1.2)/2.1+0.2)*(8.0)+(-y)))
            wr [I 137; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 138
        !"test138"
        //let z0 = (((-7.4)*(-7.3)-5.8))+(-1.2)+6.3
        //printfn "%d" <| 138
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 139
        !"test139"
        //let z0 = x
        //printfn "%d" <| 139
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 139; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 140
        !"test140"
        //let z0 = (((-1.8)*x+((-1.0)+(-7.3)-(-4.7)+y-(-8.2))))
        //printfn "%d" <| 140
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-1.8)*x+((-1.0)+(-7.3)-(-4.7)+y-(-8.2))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-1.8)*p+((-1.0)+(-7.3)-(-4.7)+q-(-8.2))))
            z2 <== (((-1.8)*x+((-1.0)+(-7.3)-(-4.7)+y-(-8.2))))
            wr [I 140; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 141
        !"test141"
        //let z0 = (7.3-y-(x)+((-y)))-0.6
        //printfn "%d" <| 141
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.3-y-(x)+((-y)))-0.6).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.3-q-(p)+((-q)))-0.6
            z2 <== (7.3-y-(x)+((-y)))-0.6
            wr [I 141; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 142
        !"test142"
        //let z0 = 1.1
        //printfn "%d" <| 142
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 143
        !"test143"
        //let z0 = (-5.3)
        //printfn "%d" <| 143
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 144
        !"test144"
        //let z0 = ((-x)-5.2/((3.8*(-2.5)-6.3)/(-x)*(x+(-x)-x*(-x)))-((x)))
        //printfn "%d" <| 144
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)-5.2/((3.8*(-2.5)-6.3)/(-x)*(x+(-x)-x*(-x)))-((x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)-5.2/((3.8*(-2.5)-6.3)/(-p)*(p+(-p)-p*(-p)))-((p)))
            z2 <== ((-x)-5.2/((3.8*(-2.5)-6.3)/(-x)*(x+(-x)-x*(-x)))-((x)))
            wr [I 144; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 145
        !"test145"
        //let z0 = 7.4
        //printfn "%d" <| 145
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 146
        !"test146"
        //let z0 = 5.6
        //printfn "%d" <| 146
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 147
        !"test147"
        //let z0 = ((-x)/((-x)/((-y)/(-y)/(-8.4)*(-x))*(0.2+7.7+(-y)/(-x))-y)/(y+3.5/8.2-1.1+((-x))+(8.6+x+(-2.0)*(-5.3)*x))-(x*6.1))
        //printfn "%d" <| 147
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)/((-x)/((-y)/(-y)/(-8.4)*(-x))*(0.2+7.7+(-y)/(-x))-y)/(y+3.5/8.2-1.1+((-x))+(8.6+x+(-2.0)*(-5.3)*x))-(x*6.1))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)/((-p)/((-q)/(-q)/(-8.4)*(-p))*(0.2+7.7+(-q)/(-p))-q)/(q+3.5/8.2-1.1+((-p))+(8.6+p+(-2.0)*(-5.3)*p))-(p*6.1))
            z2 <== ((-x)/((-x)/((-y)/(-y)/(-8.4)*(-x))*(0.2+7.7+(-y)/(-x))-y)/(y+3.5/8.2-1.1+((-x))+(8.6+x+(-2.0)*(-5.3)*x))-(x*6.1))
            wr [I 147; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 148
        !"test148"
        //let z0 = ((-8.8)*y/((-x)/((-y)-1.3-(-6.7)*x)-x-3.7/x+(-0.2)+(-3.2)-(-y))/((y/(-x)*(-x))-(3.1/(-y)*(-x)+3.8)+(-y)*x-(-y)/(-y))-((-6.0)))
        //printfn "%d" <| 148
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-8.8)*y/((-x)/((-y)-1.3-(-6.7)*x)-x-3.7/x+(-0.2)+(-3.2)-(-y))/((y/(-x)*(-x))-(3.1/(-y)*(-x)+3.8)+(-y)*x-(-y)/(-y))-((-6.0)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-8.8)*q/((-p)/((-q)-1.3-(-6.7)*p)-p-3.7/p+(-0.2)+(-3.2)-(-q))/((q/(-p)*(-p))-(3.1/(-q)*(-p)+3.8)+(-q)*p-(-q)/(-q))-((-6.0)))
            z2 <== ((-8.8)*y/((-x)/((-y)-1.3-(-6.7)*x)-x-3.7/x+(-0.2)+(-3.2)-(-y))/((y/(-x)*(-x))-(3.1/(-y)*(-x)+3.8)+(-y)*x-(-y)/(-y))-((-6.0)))
            wr [I 148; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 149
        !"test149"
        //let z0 = (-2.0)*y/(-8.8)-(-7.6)/5.3*(-6.7)/(-x)+2.7+3.7+x*((-y))
        //printfn "%d" <| 149
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-2.0)*y/(-8.8)-(-7.6)/5.3*(-6.7)/(-x)+2.7+3.7+x*((-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-2.0)*q/(-8.8)-(-7.6)/5.3*(-6.7)/(-p)+2.7+3.7+p*((-q))
            z2 <== (-2.0)*y/(-8.8)-(-7.6)/5.3*(-6.7)/(-x)+2.7+3.7+x*((-y))
            wr [I 149; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 150
        !"test150"
        //let z0 = (-x)
        //printfn "%d" <| 150
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 150; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 151
        !"test151"
        //let z0 = 4.8
        //printfn "%d" <| 151
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 152
        !"test152"
        //let z0 = (-6.1)
        //printfn "%d" <| 152
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 153
        !"test153"
        //let z0 = (-x)
        //printfn "%d" <| 153
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 153; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 154
        !"test154"
        //let z0 = y
        //printfn "%d" <| 154
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 154; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 155
        !"test155"
        //let z0 = (-1.0)
        //printfn "%d" <| 155
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 156
        !"test156"
        //let z0 = (-x)
        //printfn "%d" <| 156
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 156; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 157
        !"test157"
        //let z0 = ((3.6+(-x)/1.7/6.7)-(-y))/(((-4.8)*(-7.0)+(-0.5)/(-x)-y)*(-y))+(-4.4)+(-x)+(x-x/(-x)+((-y)*(-x))+(-1.0))
        //printfn "%d" <| 157
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((3.6+(-x)/1.7/6.7)-(-y))/(((-4.8)*(-7.0)+(-0.5)/(-x)-y)*(-y))+(-4.4)+(-x)+(x-x/(-x)+((-y)*(-x))+(-1.0))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((3.6+(-p)/1.7/6.7)-(-q))/(((-4.8)*(-7.0)+(-0.5)/(-p)-q)*(-q))+(-4.4)+(-p)+(p-p/(-p)+((-q)*(-p))+(-1.0))
            z2 <== ((3.6+(-x)/1.7/6.7)-(-y))/(((-4.8)*(-7.0)+(-0.5)/(-x)-y)*(-y))+(-4.4)+(-x)+(x-x/(-x)+((-y)*(-x))+(-1.0))
            wr [I 157; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 158
        !"test158"
        //let z0 = ((((-2.2)*4.7*5.6+(-6.7)*(-4.3))-((-4.8)*(-0.7)-5.1+6.8)-(-7.1))+((-5.5)/0.2*4.3+(-7.2)/(-5.6)-(-x)+((-1.3)-y+(-y)*(-6.0))+((-2.1)-4.0)-x)*(-x))
        //printfn "%d" <| 158
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-2.2)*4.7*5.6+(-6.7)*(-4.3))-((-4.8)*(-0.7)-5.1+6.8)-(-7.1))+((-5.5)/0.2*4.3+(-7.2)/(-5.6)-(-x)+((-1.3)-y+(-y)*(-6.0))+((-2.1)-4.0)-x)*(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-2.2)*4.7*5.6+(-6.7)*(-4.3))-((-4.8)*(-0.7)-5.1+6.8)-(-7.1))+((-5.5)/0.2*4.3+(-7.2)/(-5.6)-(-p)+((-1.3)-q+(-q)*(-6.0))+((-2.1)-4.0)-p)*(-p))
            z2 <== ((((-2.2)*4.7*5.6+(-6.7)*(-4.3))-((-4.8)*(-0.7)-5.1+6.8)-(-7.1))+((-5.5)/0.2*4.3+(-7.2)/(-5.6)-(-x)+((-1.3)-y+(-y)*(-6.0))+((-2.1)-4.0)-x)*(-x))
            wr [I 158; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 159
        !"test159"
        //let z0 = (0.3)
        //printfn "%d" <| 159
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 160
        !"test160"
        //let z0 = (8.4-x/3.7/1.7)
        //printfn "%d" <| 160
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((8.4-x/3.7/1.7)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (8.4-p/3.7/1.7)
            z2 <== (8.4-x/3.7/1.7)
            wr [I 160; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 161
        !"test161"
        //let z0 = 4.0
        //printfn "%d" <| 161
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 162
        !"test162"
        //let z0 = (-y)
        //printfn "%d" <| 162
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 162; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 163
        !"test163"
        //let z0 = 5.2*(((-x)-0.7))-x/(-x)-(-8.0)
        //printfn "%d" <| 163
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (5.2*(((-x)-0.7))-x/(-x)-(-8.0)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 5.2*(((-p)-0.7))-p/(-p)-(-8.0)
            z2 <== 5.2*(((-x)-0.7))-x/(-x)-(-8.0)
            wr [I 163; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 164
        !"test164"
        //let z0 = ((y-(-7.8)+(-y))*1.3)/(-2.1)/y
        //printfn "%d" <| 164
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y-(-7.8)+(-y))*1.3)/(-2.1)/y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q-(-7.8)+(-q))*1.3)/(-2.1)/q
            z2 <== ((y-(-7.8)+(-y))*1.3)/(-2.1)/y
            wr [I 164; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 165
        !"test165"
        //let z0 = (y+x+((-1.1)+y/((-y)-(-x)-(-3.6))/(-x)+((-6.6)/(-x)-y-(-4.7)+(-5.1))))
        //printfn "%d" <| 165
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+x+((-1.1)+y/((-y)-(-x)-(-3.6))/(-x)+((-6.6)/(-x)-y-(-4.7)+(-5.1))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+p+((-1.1)+q/((-q)-(-p)-(-3.6))/(-p)+((-6.6)/(-p)-q-(-4.7)+(-5.1))))
            z2 <== (y+x+((-1.1)+y/((-y)-(-x)-(-3.6))/(-x)+((-6.6)/(-x)-y-(-4.7)+(-5.1))))
            wr [I 165; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 166
        !"test166"
        //let z0 = (((x+(-y)+(-x)/y)/(0.1)/((-3.6)+x*y-2.8+6.6)/((-5.5)*x))+(-x)+(6.5)+((-1.8)/y)+(-x))
        //printfn "%d" <| 166
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((x+(-y)+(-x)/y)/(0.1)/((-3.6)+x*y-2.8+6.6)/((-5.5)*x))+(-x)+(6.5)+((-1.8)/y)+(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((p+(-q)+(-p)/q)/(0.1)/((-3.6)+p*q-2.8+6.6)/((-5.5)*p))+(-p)+(6.5)+((-1.8)/q)+(-p))
            z2 <== (((x+(-y)+(-x)/y)/(0.1)/((-3.6)+x*y-2.8+6.6)/((-5.5)*x))+(-x)+(6.5)+((-1.8)/y)+(-x))
            wr [I 166; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 167
        !"test167"
        //let z0 = 5.6
        //printfn "%d" <| 167
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 168
        !"test168"
        //let z0 = ((4.4+(-y))+x*(((-x)*(-1.8))))
        //printfn "%d" <| 168
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((4.4+(-y))+x*(((-x)*(-1.8))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((4.4+(-q))+p*(((-p)*(-1.8))))
            z2 <== ((4.4+(-y))+x*(((-x)*(-1.8))))
            wr [I 168; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 169
        !"test169"
        //let z0 = (-3.6)
        //printfn "%d" <| 169
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 170
        !"test170"
        //let z0 = (((-y)/y+x/(-8.1))+y/(x*(-2.0))-((y/x+0.2*6.2)+(-y)/(3.4*(-3.2)+(-y))-1.0)+(-y))
        //printfn "%d" <| 170
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)/y+x/(-8.1))+y/(x*(-2.0))-((y/x+0.2*6.2)+(-y)/(3.4*(-3.2)+(-y))-1.0)+(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)/q+p/(-8.1))+q/(p*(-2.0))-((q/p+0.2*6.2)+(-q)/(3.4*(-3.2)+(-q))-1.0)+(-q))
            z2 <== (((-y)/y+x/(-8.1))+y/(x*(-2.0))-((y/x+0.2*6.2)+(-y)/(3.4*(-3.2)+(-y))-1.0)+(-y))
            wr [I 170; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 171
        !"test171"
        //let z0 = (-x)
        //printfn "%d" <| 171
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 171; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 172
        !"test172"
        //let z0 = ((6.8*(4.7)/(-y)/(-1.7)*3.7)*(-y))
        //printfn "%d" <| 172
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((6.8*(4.7)/(-y)/(-1.7)*3.7)*(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((6.8*(4.7)/(-q)/(-1.7)*3.7)*(-q))
            z2 <== ((6.8*(4.7)/(-y)/(-1.7)*3.7)*(-y))
            wr [I 172; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 173
        !"test173"
        //let z0 = 2.2
        //printfn "%d" <| 173
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 174
        !"test174"
        //let z0 = y
        //printfn "%d" <| 174
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 174; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 175
        !"test175"
        //let z0 = 7.8
        //printfn "%d" <| 175
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 176
        !"test176"
        //let z0 = ((0.7-((-x)*(-y))/(-x)))
        //printfn "%d" <| 176
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((0.7-((-x)*(-y))/(-x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((0.7-((-p)*(-q))/(-p)))
            z2 <== ((0.7-((-x)*(-y))/(-x)))
            wr [I 176; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 177
        !"test177"
        //let z0 = (((1.7))*0.8-(-x))
        //printfn "%d" <| 177
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((1.7))*0.8-(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((1.7))*0.8-(-p))
            z2 <== (((1.7))*0.8-(-x))
            wr [I 177; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 178
        !"test178"
        //let z0 = (5.4*((-4.7)-0.5-(-x)+(-x)/(-3.6)/6.8)/(y*(-0.1))-(-2.1)+((-8.0)))
        //printfn "%d" <| 178
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.4*((-4.7)-0.5-(-x)+(-x)/(-3.6)/6.8)/(y*(-0.1))-(-2.1)+((-8.0)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.4*((-4.7)-0.5-(-p)+(-p)/(-3.6)/6.8)/(q*(-0.1))-(-2.1)+((-8.0)))
            z2 <== (5.4*((-4.7)-0.5-(-x)+(-x)/(-3.6)/6.8)/(y*(-0.1))-(-2.1)+((-8.0)))
            wr [I 178; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 179
        !"test179"
        //let z0 = (1.4*((3.4+(-y)+(-4.2)-y)*y)*(y-(-3.6))/y-((-1.1)*(x)-4.2))
        //printfn "%d" <| 179
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((1.4*((3.4+(-y)+(-4.2)-y)*y)*(y-(-3.6))/y-((-1.1)*(x)-4.2))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (1.4*((3.4+(-q)+(-4.2)-q)*q)*(q-(-3.6))/q-((-1.1)*(p)-4.2))
            z2 <== (1.4*((3.4+(-y)+(-4.2)-y)*y)*(y-(-3.6))/y-((-1.1)*(x)-4.2))
            wr [I 179; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 180
        !"test180"
        //let z0 = ((-3.4)+(-y)+y/y)
        //printfn "%d" <| 180
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-3.4)+(-y)+y/y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-3.4)+(-q)+q/q)
            z2 <== ((-3.4)+(-y)+y/y)
            wr [I 180; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 181
        !"test181"
        //let z0 = ((8.3-(-x)-(y+(-6.7)/5.6/(-y)-y)/(y*2.5*(-7.7)-6.8))/2.5*x*(x)*(-4.7))
        //printfn "%d" <| 181
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((8.3-(-x)-(y+(-6.7)/5.6/(-y)-y)/(y*2.5*(-7.7)-6.8))/2.5*x*(x)*(-4.7))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((8.3-(-p)-(q+(-6.7)/5.6/(-q)-q)/(q*2.5*(-7.7)-6.8))/2.5*p*(p)*(-4.7))
            z2 <== ((8.3-(-x)-(y+(-6.7)/5.6/(-y)-y)/(y*2.5*(-7.7)-6.8))/2.5*x*(x)*(-4.7))
            wr [I 181; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 182
        !"test182"
        //let z0 = (1.8+(-6.5)/2.8*((7.2*y-8.4+(-y)*5.6))/7.7)
        //printfn "%d" <| 182
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((1.8+(-6.5)/2.8*((7.2*y-8.4+(-y)*5.6))/7.7)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (1.8+(-6.5)/2.8*((7.2*q-8.4+(-q)*5.6))/7.7)
            z2 <== (1.8+(-6.5)/2.8*((7.2*y-8.4+(-y)*5.6))/7.7)
            wr [I 182; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 183
        !"test183"
        //let z0 = ((-1.7)/4.6+(5.1*(-x)/2.7))
        //printfn "%d" <| 183
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-1.7)/4.6+(5.1*(-x)/2.7))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-1.7)/4.6+(5.1*(-p)/2.7))
            z2 <== ((-1.7)/4.6+(5.1*(-x)/2.7))
            wr [I 183; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 184
        !"test184"
        //let z0 = (-x)
        //printfn "%d" <| 184
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 184; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 185
        !"test185"
        //let z0 = (8.3+(y/(-x)-x)-y+((-3.6)-(-y))-y+x/1.5-(-6.3)/5.1+0.4-(-7.6)/y/(x/((-x)+0.5))/1.1-x)
        //printfn "%d" <| 185
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((8.3+(y/(-x)-x)-y+((-3.6)-(-y))-y+x/1.5-(-6.3)/5.1+0.4-(-7.6)/y/(x/((-x)+0.5))/1.1-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (8.3+(q/(-p)-p)-q+((-3.6)-(-q))-q+p/1.5-(-6.3)/5.1+0.4-(-7.6)/q/(p/((-p)+0.5))/1.1-p)
            z2 <== (8.3+(y/(-x)-x)-y+((-3.6)-(-y))-y+x/1.5-(-6.3)/5.1+0.4-(-7.6)/y/(x/((-x)+0.5))/1.1-x)
            wr [I 185; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 186
        !"test186"
        //let z0 = (-y)-(-x)+(-8.8)-(-x)
        //printfn "%d" <| 186
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)-(-x)+(-8.8)-(-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)-(-p)+(-8.8)-(-p)
            z2 <== (-y)-(-x)+(-8.8)-(-x)
            wr [I 186; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 187
        !"test187"
        //let z0 = 6.6
        //printfn "%d" <| 187
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 188
        !"test188"
        //let z0 = ((y-(-x)+6.0)/8.7)
        //printfn "%d" <| 188
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y-(-x)+6.0)/8.7)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q-(-p)+6.0)/8.7)
            z2 <== ((y-(-x)+6.0)/8.7)
            wr [I 188; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 189
        !"test189"
        //let z0 = ((-7.3)+(-4.2))
        //printfn "%d" <| 189
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 190
        !"test190"
        //let z0 = 5.8
        //printfn "%d" <| 190
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 191
        !"test191"
        //let z0 = 1.8
        //printfn "%d" <| 191
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 192
        !"test192"
        //let z0 = (-6.0)
        //printfn "%d" <| 192
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 193
        !"test193"
        //let z0 = y
        //printfn "%d" <| 193
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 193; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 194
        !"test194"
        //let z0 = (x/(x))
        //printfn "%d" <| 194
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x/(x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p/(p))
            z2 <== (x/(x))
            wr [I 194; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 195
        !"test195"
        //let z0 = (((-0.2)+(4.1-2.1+7.0/x+(-x)))+(-x)*(-y)*6.3-(8.2/6.4/(-y)*(-x)-x))
        //printfn "%d" <| 195
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-0.2)+(4.1-2.1+7.0/x+(-x)))+(-x)*(-y)*6.3-(8.2/6.4/(-y)*(-x)-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-0.2)+(4.1-2.1+7.0/p+(-p)))+(-p)*(-q)*6.3-(8.2/6.4/(-q)*(-p)-p))
            z2 <== (((-0.2)+(4.1-2.1+7.0/x+(-x)))+(-x)*(-y)*6.3-(8.2/6.4/(-y)*(-x)-x))
            wr [I 195; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 196
        !"test196"
        //let z0 = ((((-3.6)*y))+y-((-x))/(-x)/(-7.1))
        //printfn "%d" <| 196
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-3.6)*y))+y-((-x))/(-x)/(-7.1))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-3.6)*q))+q-((-p))/(-p)/(-7.1))
            z2 <== ((((-3.6)*y))+y-((-x))/(-x)/(-7.1))
            wr [I 196; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 197
        !"test197"
        //let z0 = (-y)
        //printfn "%d" <| 197
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 197; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 198
        !"test198"
        //let z0 = ((((-4.0)*8.2/(-5.7)/7.1-(-x))/(-1.0)+(-y)+y*(y/(-7.7)*y+0.7))*y+(((-6.4)*6.0)))
        //printfn "%d" <| 198
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-4.0)*8.2/(-5.7)/7.1-(-x))/(-1.0)+(-y)+y*(y/(-7.7)*y+0.7))*y+(((-6.4)*6.0)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-4.0)*8.2/(-5.7)/7.1-(-p))/(-1.0)+(-q)+q*(q/(-7.7)*q+0.7))*q+(((-6.4)*6.0)))
            z2 <== ((((-4.0)*8.2/(-5.7)/7.1-(-x))/(-1.0)+(-y)+y*(y/(-7.7)*y+0.7))*y+(((-6.4)*6.0)))
            wr [I 198; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 199
        !"test199"
        //let z0 = ((-2.5))
        //printfn "%d" <| 199
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 200
        !"test200"
        //let z0 = ((-y))
        //printfn "%d" <| 200
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q))
            z2 <== ((-y))
            wr [I 200; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 201
        !"test201"
        //let z0 = 5.5
        //printfn "%d" <| 201
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 202
        !"test202"
        //let z0 = ((((-x))+y-2.8/(-5.0)-(-x)*x*(-y))*(-x)/((y/(-y)+(-4.0))+0.2*(-x)+((-2.4)/(-8.6)-y-4.2+(-3.5)))*(-6.4))
        //printfn "%d" <| 202
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-x))+y-2.8/(-5.0)-(-x)*x*(-y))*(-x)/((y/(-y)+(-4.0))+0.2*(-x)+((-2.4)/(-8.6)-y-4.2+(-3.5)))*(-6.4))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-p))+q-2.8/(-5.0)-(-p)*p*(-q))*(-p)/((q/(-q)+(-4.0))+0.2*(-p)+((-2.4)/(-8.6)-q-4.2+(-3.5)))*(-6.4))
            z2 <== ((((-x))+y-2.8/(-5.0)-(-x)*x*(-y))*(-x)/((y/(-y)+(-4.0))+0.2*(-x)+((-2.4)/(-8.6)-y-4.2+(-3.5)))*(-6.4))
            wr [I 202; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 203
        !"test203"
        //let z0 = 0.8
        //printfn "%d" <| 203
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 204
        !"test204"
        //let z0 = 6.0
        //printfn "%d" <| 204
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 205
        !"test205"
        //let z0 = (((-x)/((-3.0)+3.7-(-5.3))))
        //printfn "%d" <| 205
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)/((-3.0)+3.7-(-5.3))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)/((-3.0)+3.7-(-5.3))))
            z2 <== (((-x)/((-3.0)+3.7-(-5.3))))
            wr [I 205; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 206
        !"test206"
        //let z0 = (((-y)*x+y/((-x)-(-x)+y+0.2*x))-y+(y+(-2.1)/(3.5)+((-0.1))*(-y))*5.3-(-y)/0.0/5.3/0.3)
        //printfn "%d" <| 206
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)*x+y/((-x)-(-x)+y+0.2*x))-y+(y+(-2.1)/(3.5)+((-0.1))*(-y))*5.3-(-y)/0.0/5.3/0.3)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)*p+q/((-p)-(-p)+q+0.2*p))-q+(q+(-2.1)/(3.5)+((-0.1))*(-q))*5.3-(-q)/0.0/5.3/0.3)
            z2 <== (((-y)*x+y/((-x)-(-x)+y+0.2*x))-y+(y+(-2.1)/(3.5)+((-0.1))*(-y))*5.3-(-y)/0.0/5.3/0.3)
            wr [I 206; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 207
        !"test207"
        //let z0 = (-8.4)
        //printfn "%d" <| 207
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 208
        !"test208"
        //let z0 = (((-x)-(-y))-((-y)*3.1+y-5.4-(-x))*((-x)/6.0+7.4+(y+(-3.5)*(-x)+(-6.3)))+(((-0.3)+7.0)*(y*x/(-2.5)-1.1)+8.8)-4.1)
        //printfn "%d" <| 208
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)-(-y))-((-y)*3.1+y-5.4-(-x))*((-x)/6.0+7.4+(y+(-3.5)*(-x)+(-6.3)))+(((-0.3)+7.0)*(y*x/(-2.5)-1.1)+8.8)-4.1)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)-(-q))-((-q)*3.1+q-5.4-(-p))*((-p)/6.0+7.4+(q+(-3.5)*(-p)+(-6.3)))+(((-0.3)+7.0)*(q*p/(-2.5)-1.1)+8.8)-4.1)
            z2 <== (((-x)-(-y))-((-y)*3.1+y-5.4-(-x))*((-x)/6.0+7.4+(y+(-3.5)*(-x)+(-6.3)))+(((-0.3)+7.0)*(y*x/(-2.5)-1.1)+8.8)-4.1)
            wr [I 208; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 209
        !"test209"
        //let z0 = 1.8
        //printfn "%d" <| 209
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 210
        !"test210"
        //let z0 = (-x)
        //printfn "%d" <| 210
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 210; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 211
        !"test211"
        //let z0 = (1.4)
        //printfn "%d" <| 211
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 212
        !"test212"
        //let z0 = (((4.5)*((-6.7)-8.8)/((-2.6)-(-y)+y/y*(-y)))-(2.8)-(-y)*0.8)
        //printfn "%d" <| 212
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((4.5)*((-6.7)-8.8)/((-2.6)-(-y)+y/y*(-y)))-(2.8)-(-y)*0.8)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((4.5)*((-6.7)-8.8)/((-2.6)-(-q)+q/q*(-q)))-(2.8)-(-q)*0.8)
            z2 <== (((4.5)*((-6.7)-8.8)/((-2.6)-(-y)+y/y*(-y)))-(2.8)-(-y)*0.8)
            wr [I 212; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 213
        !"test213"
        //let z0 = (-4.5)
        //printfn "%d" <| 213
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 214
        !"test214"
        //let z0 = 7.6
        //printfn "%d" <| 214
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 215
        !"test215"
        //let z0 = 1.5
        //printfn "%d" <| 215
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 216
        !"test216"
        //let z0 = (x-((y/8.3-(-7.2)*0.7)+((-x)-(-x)+3.2+(-y))*(-x))*((2.5/(-x)+y+y+(-5.5))+((-3.0)-(-x))-(-x)/5.7/(-y)+x/(-x))/x)
        //printfn "%d" <| 216
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x-((y/8.3-(-7.2)*0.7)+((-x)-(-x)+3.2+(-y))*(-x))*((2.5/(-x)+y+y+(-5.5))+((-3.0)-(-x))-(-x)/5.7/(-y)+x/(-x))/x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p-((q/8.3-(-7.2)*0.7)+((-p)-(-p)+3.2+(-q))*(-p))*((2.5/(-p)+q+q+(-5.5))+((-3.0)-(-p))-(-p)/5.7/(-q)+p/(-p))/p)
            z2 <== (x-((y/8.3-(-7.2)*0.7)+((-x)-(-x)+3.2+(-y))*(-x))*((2.5/(-x)+y+y+(-5.5))+((-3.0)-(-x))-(-x)/5.7/(-y)+x/(-x))/x)
            wr [I 216; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 217
        !"test217"
        //let z0 = ((-y)+y/y/(-x)+5.6+(((-x)+(-7.6)/2.7+(-5.5)+y)-x/((-8.7)*8.1/(-x))-(-x)/(-1.2))*((-x)/((-x)/(-3.6)-4.7)-(-y)))
        //printfn "%d" <| 217
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)+y/y/(-x)+5.6+(((-x)+(-7.6)/2.7+(-5.5)+y)-x/((-8.7)*8.1/(-x))-(-x)/(-1.2))*((-x)/((-x)/(-3.6)-4.7)-(-y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)+q/q/(-p)+5.6+(((-p)+(-7.6)/2.7+(-5.5)+q)-p/((-8.7)*8.1/(-p))-(-p)/(-1.2))*((-p)/((-p)/(-3.6)-4.7)-(-q)))
            z2 <== ((-y)+y/y/(-x)+5.6+(((-x)+(-7.6)/2.7+(-5.5)+y)-x/((-8.7)*8.1/(-x))-(-x)/(-1.2))*((-x)/((-x)/(-3.6)-4.7)-(-y)))
            wr [I 217; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 218
        !"test218"
        //let z0 = ((-5.1)+((-y)/6.5*(-x)*((-0.6)/(-4.3)-(-5.7)/4.7-y))/(-6.1))
        //printfn "%d" <| 218
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-5.1)+((-y)/6.5*(-x)*((-0.6)/(-4.3)-(-5.7)/4.7-y))/(-6.1))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-5.1)+((-q)/6.5*(-p)*((-0.6)/(-4.3)-(-5.7)/4.7-q))/(-6.1))
            z2 <== ((-5.1)+((-y)/6.5*(-x)*((-0.6)/(-4.3)-(-5.7)/4.7-y))/(-6.1))
            wr [I 218; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 219
        !"test219"
        //let z0 = ((-0.7)-x*(x/(-4.4)/(-y)*6.5+(-5.8))-8.1*(((-y)/(-7.2)+(-5.1))))
        //printfn "%d" <| 219
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.7)-x*(x/(-4.4)/(-y)*6.5+(-5.8))-8.1*(((-y)/(-7.2)+(-5.1))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.7)-p*(p/(-4.4)/(-q)*6.5+(-5.8))-8.1*(((-q)/(-7.2)+(-5.1))))
            z2 <== ((-0.7)-x*(x/(-4.4)/(-y)*6.5+(-5.8))-8.1*(((-y)/(-7.2)+(-5.1))))
            wr [I 219; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 220
        !"test220"
        //let z0 = 8.4+y-(-x)*x+((y*y-y)/((-x)-x/6.1-(-5.7)/(-5.4)))/(-4.3)
        //printfn "%d" <| 220
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (8.4+y-(-x)*x+((y*y-y)/((-x)-x/6.1-(-5.7)/(-5.4)))/(-4.3)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 8.4+q-(-p)*p+((q*q-q)/((-p)-p/6.1-(-5.7)/(-5.4)))/(-4.3)
            z2 <== 8.4+y-(-x)*x+((y*y-y)/((-x)-x/6.1-(-5.7)/(-5.4)))/(-4.3)
            wr [I 220; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 221
        !"test221"
        //let z0 = (x/(-y)/(2.6*((-x)-4.8/(-7.0)))*y/(y-(-y))-y*(6.0*7.2/y*x/7.5)+(x))
        //printfn "%d" <| 221
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x/(-y)/(2.6*((-x)-4.8/(-7.0)))*y/(y-(-y))-y*(6.0*7.2/y*x/7.5)+(x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p/(-q)/(2.6*((-p)-4.8/(-7.0)))*q/(q-(-q))-q*(6.0*7.2/q*p/7.5)+(p))
            z2 <== (x/(-y)/(2.6*((-x)-4.8/(-7.0)))*y/(y-(-y))-y*(6.0*7.2/y*x/7.5)+(x))
            wr [I 221; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 222
        !"test222"
        //let z0 = (-2.3)
        //printfn "%d" <| 222
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 223
        !"test223"
        //let z0 = (((-x))*((x/(-y)*1.7*y)*((-1.4)-8.2-x)*(-y)*0.8+7.0+(-y)-6.2))
        //printfn "%d" <| 223
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x))*((x/(-y)*1.7*y)*((-1.4)-8.2-x)*(-y)*0.8+7.0+(-y)-6.2))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p))*((p/(-q)*1.7*q)*((-1.4)-8.2-p)*(-q)*0.8+7.0+(-q)-6.2))
            z2 <== (((-x))*((x/(-y)*1.7*y)*((-1.4)-8.2-x)*(-y)*0.8+7.0+(-y)-6.2))
            wr [I 223; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 224
        !"test224"
        //let z0 = y
        //printfn "%d" <| 224
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 224; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 225
        !"test225"
        //let z0 = ((-x)/(((-x))-7.0/x-y/6.4))
        //printfn "%d" <| 225
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)/(((-x))-7.0/x-y/6.4))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)/(((-p))-7.0/p-q/6.4))
            z2 <== ((-x)/(((-x))-7.0/x-y/6.4))
            wr [I 225; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 226
        !"test226"
        //let z0 = 2.8
        //printfn "%d" <| 226
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 227
        !"test227"
        //let z0 = ((6.0+(-x)+(-5.7)))
        //printfn "%d" <| 227
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((6.0+(-x)+(-5.7)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((6.0+(-p)+(-5.7)))
            z2 <== ((6.0+(-x)+(-5.7)))
            wr [I 227; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 228
        !"test228"
        //let z0 = (-2.1)
        //printfn "%d" <| 228
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 229
        !"test229"
        //let z0 = ((-7.4)*(-7.6))+(8.4)*((-7.3)*(-x)-x*(-y)*6.5+4.6-(-8.4))
        //printfn "%d" <| 229
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.4)*(-7.6))+(8.4)*((-7.3)*(-x)-x*(-y)*6.5+4.6-(-8.4))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.4)*(-7.6))+(8.4)*((-7.3)*(-p)-p*(-q)*6.5+4.6-(-8.4))
            z2 <== ((-7.4)*(-7.6))+(8.4)*((-7.3)*(-x)-x*(-y)*6.5+4.6-(-8.4))
            wr [I 229; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 230
        !"test230"
        //let z0 = (-y)
        //printfn "%d" <| 230
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 230; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 231
        !"test231"
        //let z0 = 1.6
        //printfn "%d" <| 231
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 232
        !"test232"
        //let z0 = (((-0.2)))
        //printfn "%d" <| 232
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 233
        !"test233"
        //let z0 = (1.1*(-x)*y+x-(7.1*7.2+y-(-y)*(-1.3))*6.4+(2.3/x+(-8.4)+x)/((-y)*3.2*(-y))*0.5-((-x)-y-(-y)))
        //printfn "%d" <| 233
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((1.1*(-x)*y+x-(7.1*7.2+y-(-y)*(-1.3))*6.4+(2.3/x+(-8.4)+x)/((-y)*3.2*(-y))*0.5-((-x)-y-(-y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (1.1*(-p)*q+p-(7.1*7.2+q-(-q)*(-1.3))*6.4+(2.3/p+(-8.4)+p)/((-q)*3.2*(-q))*0.5-((-p)-q-(-q)))
            z2 <== (1.1*(-x)*y+x-(7.1*7.2+y-(-y)*(-1.3))*6.4+(2.3/x+(-8.4)+x)/((-y)*3.2*(-y))*0.5-((-x)-y-(-y)))
            wr [I 233; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 234
        !"test234"
        //let z0 = (((-2.3)/(x*y+(-x)/(-4.6)/(-2.0))*(-5.6)/(-y)/((-y)))-x-(-6.1)+(x-x*(-3.8)*y)*((-x)/(-7.5)/y)+4.1-(x)*(-0.8)+(-x))
        //printfn "%d" <| 234
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-2.3)/(x*y+(-x)/(-4.6)/(-2.0))*(-5.6)/(-y)/((-y)))-x-(-6.1)+(x-x*(-3.8)*y)*((-x)/(-7.5)/y)+4.1-(x)*(-0.8)+(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-2.3)/(p*q+(-p)/(-4.6)/(-2.0))*(-5.6)/(-q)/((-q)))-p-(-6.1)+(p-p*(-3.8)*q)*((-p)/(-7.5)/q)+4.1-(p)*(-0.8)+(-p))
            z2 <== (((-2.3)/(x*y+(-x)/(-4.6)/(-2.0))*(-5.6)/(-y)/((-y)))-x-(-6.1)+(x-x*(-3.8)*y)*((-x)/(-7.5)/y)+4.1-(x)*(-0.8)+(-x))
            wr [I 234; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 235
        !"test235"
        //let z0 = (5.5+(2.3+(-5.5))+y)
        //printfn "%d" <| 235
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.5+(2.3+(-5.5))+y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.5+(2.3+(-5.5))+q)
            z2 <== (5.5+(2.3+(-5.5))+y)
            wr [I 235; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 236
        !"test236"
        //let z0 = 1.1
        //printfn "%d" <| 236
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 237
        !"test237"
        //let z0 = 1.7
        //printfn "%d" <| 237
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 238
        !"test238"
        //let z0 = ((-1.8)*(2.7-(-1.4)-0.1/y-(-3.8)/2.3+((-2.3))*x+(2.2*(-8.3)-x-x)))
        //printfn "%d" <| 238
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-1.8)*(2.7-(-1.4)-0.1/y-(-3.8)/2.3+((-2.3))*x+(2.2*(-8.3)-x-x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-1.8)*(2.7-(-1.4)-0.1/q-(-3.8)/2.3+((-2.3))*p+(2.2*(-8.3)-p-p)))
            z2 <== ((-1.8)*(2.7-(-1.4)-0.1/y-(-3.8)/2.3+((-2.3))*x+(2.2*(-8.3)-x-x)))
            wr [I 238; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 239
        !"test239"
        //let z0 = y
        //printfn "%d" <| 239
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 239; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 240
        !"test240"
        //let z0 = (-0.5)
        //printfn "%d" <| 240
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 241
        !"test241"
        //let z0 = 0.2
        //printfn "%d" <| 241
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 242
        !"test242"
        //let z0 = y
        //printfn "%d" <| 242
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 242; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 243
        !"test243"
        //let z0 = 8.1
        //printfn "%d" <| 243
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 244
        !"test244"
        //let z0 = (-y)
        //printfn "%d" <| 244
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 244; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 245
        !"test245"
        //let z0 = (((-y))-(-2.2))
        //printfn "%d" <| 245
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y))-(-2.2))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q))-(-2.2))
            z2 <== (((-y))-(-2.2))
            wr [I 245; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 246
        !"test246"
        //let z0 = (-y)*(0.7/(3.7*4.7*y*x*(-3.4))/(-2.6)/(-6.7)-y)/(-y)+x
        //printfn "%d" <| 246
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)*(0.7/(3.7*4.7*y*x*(-3.4))/(-2.6)/(-6.7)-y)/(-y)+x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)*(0.7/(3.7*4.7*q*p*(-3.4))/(-2.6)/(-6.7)-q)/(-q)+p
            z2 <== (-y)*(0.7/(3.7*4.7*y*x*(-3.4))/(-2.6)/(-6.7)-y)/(-y)+x
            wr [I 246; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 247
        !"test247"
        //let z0 = 2.8
        //printfn "%d" <| 247
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 248
        !"test248"
        //let z0 = 1.4
        //printfn "%d" <| 248
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 249
        !"test249"
        //let z0 = 4.5
        //printfn "%d" <| 249
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 250
        !"test250"
        //let z0 = (3.1+((8.2*(-2.5)*(-x)*6.0)))
        //printfn "%d" <| 250
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((3.1+((8.2*(-2.5)*(-x)*6.0)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (3.1+((8.2*(-2.5)*(-p)*6.0)))
            z2 <== (3.1+((8.2*(-2.5)*(-x)*6.0)))
            wr [I 250; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 251
        !"test251"
        //let z0 = (-y)
        //printfn "%d" <| 251
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 251; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 252
        !"test252"
        //let z0 = 8.2
        //printfn "%d" <| 252
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 253
        !"test253"
        //let z0 = 2.3
        //printfn "%d" <| 253
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 254
        !"test254"
        //let z0 = (-5.7)/(-x)-((-1.4)*((-x)/(-2.0)*y-3.5/(-x))/(0.6-(-y)*5.5))-(-y)
        //printfn "%d" <| 254
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-5.7)/(-x)-((-1.4)*((-x)/(-2.0)*y-3.5/(-x))/(0.6-(-y)*5.5))-(-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-5.7)/(-p)-((-1.4)*((-p)/(-2.0)*q-3.5/(-p))/(0.6-(-q)*5.5))-(-q)
            z2 <== (-5.7)/(-x)-((-1.4)*((-x)/(-2.0)*y-3.5/(-x))/(0.6-(-y)*5.5))-(-y)
            wr [I 254; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 255
        !"test255"
        //let z0 = (-1.7)
        //printfn "%d" <| 255
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 256
        !"test256"
        //let z0 = ((((-y)-y*(-0.1)/(-1.7))))
        //printfn "%d" <| 256
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-y)-y*(-0.1)/(-1.7))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-q)-q*(-0.1)/(-1.7))))
            z2 <== ((((-y)-y*(-0.1)/(-1.7))))
            wr [I 256; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 257
        !"test257"
        //let z0 = ((-4.6)/x/8.0/4.6)
        //printfn "%d" <| 257
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-4.6)/x/8.0/4.6)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-4.6)/p/8.0/4.6)
            z2 <== ((-4.6)/x/8.0/4.6)
            wr [I 257; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 258
        !"test258"
        //let z0 = ((-x)-(8.2*(-0.8)*((-2.4)+(-y)/x))-(-x)-x/(((-x)-y)-4.3*6.4*(-y)*((-7.1)-y/x-6.0)-(-7.4)))
        //printfn "%d" <| 258
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)-(8.2*(-0.8)*((-2.4)+(-y)/x))-(-x)-x/(((-x)-y)-4.3*6.4*(-y)*((-7.1)-y/x-6.0)-(-7.4)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)-(8.2*(-0.8)*((-2.4)+(-q)/p))-(-p)-p/(((-p)-q)-4.3*6.4*(-q)*((-7.1)-q/p-6.0)-(-7.4)))
            z2 <== ((-x)-(8.2*(-0.8)*((-2.4)+(-y)/x))-(-x)-x/(((-x)-y)-4.3*6.4*(-y)*((-7.1)-y/x-6.0)-(-7.4)))
            wr [I 258; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 259
        !"test259"
        //let z0 = (-7.8)
        //printfn "%d" <| 259
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 260
        !"test260"
        //let z0 = (((6.2+(-4.1)/y+y)/(-0.3)-(-x))-(-y)/y)
        //printfn "%d" <| 260
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((6.2+(-4.1)/y+y)/(-0.3)-(-x))-(-y)/y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((6.2+(-4.1)/q+q)/(-0.3)-(-p))-(-q)/q)
            z2 <== (((6.2+(-4.1)/y+y)/(-0.3)-(-x))-(-y)/y)
            wr [I 260; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 261
        !"test261"
        //let z0 = y
        //printfn "%d" <| 261
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 261; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 262
        !"test262"
        //let z0 = 0.8
        //printfn "%d" <| 262
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 263
        !"test263"
        //let z0 = y
        //printfn "%d" <| 263
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 263; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 264
        !"test264"
        //let z0 = (-x)
        //printfn "%d" <| 264
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 264; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 265
        !"test265"
        //let z0 = y
        //printfn "%d" <| 265
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 265; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 266
        !"test266"
        //let z0 = (-6.7)
        //printfn "%d" <| 266
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 267
        !"test267"
        //let z0 = (((-y)/((-x)*(-4.4)/(-2.0)+(-2.8)-(-x))+1.6*(-0.3)/(-y))*((-0.4)+(-x)*(-x))/(-y)*0.1+x*(-x)-((-x)*y/x*(-2.6)-(-8.6)/(-4.5))/3.3)
        //printfn "%d" <| 267
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)/((-x)*(-4.4)/(-2.0)+(-2.8)-(-x))+1.6*(-0.3)/(-y))*((-0.4)+(-x)*(-x))/(-y)*0.1+x*(-x)-((-x)*y/x*(-2.6)-(-8.6)/(-4.5))/3.3)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)/((-p)*(-4.4)/(-2.0)+(-2.8)-(-p))+1.6*(-0.3)/(-q))*((-0.4)+(-p)*(-p))/(-q)*0.1+p*(-p)-((-p)*q/p*(-2.6)-(-8.6)/(-4.5))/3.3)
            z2 <== (((-y)/((-x)*(-4.4)/(-2.0)+(-2.8)-(-x))+1.6*(-0.3)/(-y))*((-0.4)+(-x)*(-x))/(-y)*0.1+x*(-x)-((-x)*y/x*(-2.6)-(-8.6)/(-4.5))/3.3)
            wr [I 267; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 268
        !"test268"
        //let z0 = (-x)
        //printfn "%d" <| 268
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 268; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 269
        !"test269"
        //let z0 = (-x)
        //printfn "%d" <| 269
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 269; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 270
        !"test270"
        //let z0 = (((0.2*(-4.2)+(-x)-(-1.1)+(-x))-(-8.7)*x+(8.0)+(3.2))/(6.6+x/x-(-8.2)+y)+(5.0+(-y)*1.1*5.5)+(5.7-6.0)-(-x)-(8.3/((-0.4)+2.4)+(-4.1)*(1.1-(-2.8)/(-8.1)+7.4/x)+(y*(-7.2)/(-7.1))))
        //printfn "%d" <| 270
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((0.2*(-4.2)+(-x)-(-1.1)+(-x))-(-8.7)*x+(8.0)+(3.2))/(6.6+x/x-(-8.2)+y)+(5.0+(-y)*1.1*5.5)+(5.7-6.0)-(-x)-(8.3/((-0.4)+2.4)+(-4.1)*(1.1-(-2.8)/(-8.1)+7.4/x)+(y*(-7.2)/(-7.1))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((0.2*(-4.2)+(-p)-(-1.1)+(-p))-(-8.7)*p+(8.0)+(3.2))/(6.6+p/p-(-8.2)+q)+(5.0+(-q)*1.1*5.5)+(5.7-6.0)-(-p)-(8.3/((-0.4)+2.4)+(-4.1)*(1.1-(-2.8)/(-8.1)+7.4/p)+(q*(-7.2)/(-7.1))))
            z2 <== (((0.2*(-4.2)+(-x)-(-1.1)+(-x))-(-8.7)*x+(8.0)+(3.2))/(6.6+x/x-(-8.2)+y)+(5.0+(-y)*1.1*5.5)+(5.7-6.0)-(-x)-(8.3/((-0.4)+2.4)+(-4.1)*(1.1-(-2.8)/(-8.1)+7.4/x)+(y*(-7.2)/(-7.1))))
            wr [I 270; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 271
        !"test271"
        //let z0 = x
        //printfn "%d" <| 271
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 271; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 272
        !"test272"
        //let z0 = (-y)
        //printfn "%d" <| 272
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 272; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 273
        !"test273"
        //let z0 = (((-7.4)/((-4.6)*(-6.4)*8.7*y+(-7.2))+x+(-y)*(-4.6)/1.7+(-2.8)-(-x)*(-0.6))+((7.6)+y/(-7.1)-(-7.4)*x+(-2.0))-4.6)
        //printfn "%d" <| 273
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-7.4)/((-4.6)*(-6.4)*8.7*y+(-7.2))+x+(-y)*(-4.6)/1.7+(-2.8)-(-x)*(-0.6))+((7.6)+y/(-7.1)-(-7.4)*x+(-2.0))-4.6)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-7.4)/((-4.6)*(-6.4)*8.7*q+(-7.2))+p+(-q)*(-4.6)/1.7+(-2.8)-(-p)*(-0.6))+((7.6)+q/(-7.1)-(-7.4)*p+(-2.0))-4.6)
            z2 <== (((-7.4)/((-4.6)*(-6.4)*8.7*y+(-7.2))+x+(-y)*(-4.6)/1.7+(-2.8)-(-x)*(-0.6))+((7.6)+y/(-7.1)-(-7.4)*x+(-2.0))-4.6)
            wr [I 273; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 274
        !"test274"
        //let z0 = 1.0-(x+(-5.7)*x-y+x*(-x)-6.5-y/(2.2))+(x+(-y)+(-8.7))
        //printfn "%d" <| 274
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (1.0-(x+(-5.7)*x-y+x*(-x)-6.5-y/(2.2))+(x+(-y)+(-8.7))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 1.0-(p+(-5.7)*p-q+p*(-p)-6.5-q/(2.2))+(p+(-q)+(-8.7))
            z2 <== 1.0-(x+(-5.7)*x-y+x*(-x)-6.5-y/(2.2))+(x+(-y)+(-8.7))
            wr [I 274; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 275
        !"test275"
        //let z0 = x
        //printfn "%d" <| 275
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 275; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 276
        !"test276"
        //let z0 = x
        //printfn "%d" <| 276
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 276; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 277
        !"test277"
        //let z0 = (-x)
        //printfn "%d" <| 277
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 277; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 278
        !"test278"
        //let z0 = (y)
        //printfn "%d" <| 278
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q)
            z2 <== (y)
            wr [I 278; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 279
        !"test279"
        //let z0 = (6.8-((-0.6)/3.0/(-y)+x-(-5.0))/((-y)*(-8.0)-(-0.6)/x)*(-7.3)/(-0.1))
        //printfn "%d" <| 279
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((6.8-((-0.6)/3.0/(-y)+x-(-5.0))/((-y)*(-8.0)-(-0.6)/x)*(-7.3)/(-0.1))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (6.8-((-0.6)/3.0/(-q)+p-(-5.0))/((-q)*(-8.0)-(-0.6)/p)*(-7.3)/(-0.1))
            z2 <== (6.8-((-0.6)/3.0/(-y)+x-(-5.0))/((-y)*(-8.0)-(-0.6)/x)*(-7.3)/(-0.1))
            wr [I 279; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 280
        !"test280"
        //let z0 = (((-x)+y*(-4.4)/(-y)*y+y-(1.0+(-8.0)*(-2.0)))/(-x)+(3.1/(-1.1))+((-y)-(x-7.8)/(-0.2))*(((-y)/(-4.2)+(-x)*(-x))+(6.0*(-4.2))))
        //printfn "%d" <| 280
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)+y*(-4.4)/(-y)*y+y-(1.0+(-8.0)*(-2.0)))/(-x)+(3.1/(-1.1))+((-y)-(x-7.8)/(-0.2))*(((-y)/(-4.2)+(-x)*(-x))+(6.0*(-4.2))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)+q*(-4.4)/(-q)*q+q-(1.0+(-8.0)*(-2.0)))/(-p)+(3.1/(-1.1))+((-q)-(p-7.8)/(-0.2))*(((-q)/(-4.2)+(-p)*(-p))+(6.0*(-4.2))))
            z2 <== (((-x)+y*(-4.4)/(-y)*y+y-(1.0+(-8.0)*(-2.0)))/(-x)+(3.1/(-1.1))+((-y)-(x-7.8)/(-0.2))*(((-y)/(-4.2)+(-x)*(-x))+(6.0*(-4.2))))
            wr [I 280; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 281
        !"test281"
        //let z0 = (-3.4)
        //printfn "%d" <| 281
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 282
        !"test282"
        //let z0 = (-3.4)
        //printfn "%d" <| 282
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 283
        !"test283"
        //let z0 = ((-x)-(4.2*x+x/1.3)*y-1.8-(-y)-(8.1+x+y/(-y)*(-x)/(-6.3)/x-5.3))
        //printfn "%d" <| 283
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)-(4.2*x+x/1.3)*y-1.8-(-y)-(8.1+x+y/(-y)*(-x)/(-6.3)/x-5.3))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)-(4.2*p+p/1.3)*q-1.8-(-q)-(8.1+p+q/(-q)*(-p)/(-6.3)/p-5.3))
            z2 <== ((-x)-(4.2*x+x/1.3)*y-1.8-(-y)-(8.1+x+y/(-y)*(-x)/(-6.3)/x-5.3))
            wr [I 283; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 284
        !"test284"
        //let z0 = (-3.7)
        //printfn "%d" <| 284
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 285
        !"test285"
        //let z0 = 1.2
        //printfn "%d" <| 285
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 286
        !"test286"
        //let z0 = (((-x)/(-x))+((-5.8)-(-y)+(-2.3))+4.6*((-y)+x+5.6+x/x)-(-5.2))+y/((8.6+6.5/(-3.8))-(-x)-(-6.8)-(-x)*(-5.7))-((-4.3)-(x-7.3*8.4)*(-6.3)*(-y))/x
        //printfn "%d" <| 286
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)/(-x))+((-5.8)-(-y)+(-2.3))+4.6*((-y)+x+5.6+x/x)-(-5.2))+y/((8.6+6.5/(-3.8))-(-x)-(-6.8)-(-x)*(-5.7))-((-4.3)-(x-7.3*8.4)*(-6.3)*(-y))/x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)/(-p))+((-5.8)-(-q)+(-2.3))+4.6*((-q)+p+5.6+p/p)-(-5.2))+q/((8.6+6.5/(-3.8))-(-p)-(-6.8)-(-p)*(-5.7))-((-4.3)-(p-7.3*8.4)*(-6.3)*(-q))/p
            z2 <== (((-x)/(-x))+((-5.8)-(-y)+(-2.3))+4.6*((-y)+x+5.6+x/x)-(-5.2))+y/((8.6+6.5/(-3.8))-(-x)-(-6.8)-(-x)*(-5.7))-((-4.3)-(x-7.3*8.4)*(-6.3)*(-y))/x
            wr [I 286; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 287
        !"test287"
        //let z0 = ((-0.2))-(-5.5)+(x+2.2)
        //printfn "%d" <| 287
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.2))-(-5.5)+(x+2.2)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.2))-(-5.5)+(p+2.2)
            z2 <== ((-0.2))-(-5.5)+(x+2.2)
            wr [I 287; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 288
        !"test288"
        //let z0 = (-3.3)
        //printfn "%d" <| 288
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 289
        !"test289"
        //let z0 = y-(-6.8)-(-1.2)
        //printfn "%d" <| 289
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y-(-6.8)-(-1.2)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q-(-6.8)-(-1.2)
            z2 <== y-(-6.8)-(-1.2)
            wr [I 289; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 290
        !"test290"
        //let z0 = 7.8
        //printfn "%d" <| 290
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 291
        !"test291"
        //let z0 = (((-x)-(-x)*y-5.1+(-x))-(-3.4)*((-0.6))-((-y)/(-2.3)-(-4.6))*(1.7*0.2/(-6.0)-x-x)+0.4*y)
        //printfn "%d" <| 291
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)-(-x)*y-5.1+(-x))-(-3.4)*((-0.6))-((-y)/(-2.3)-(-4.6))*(1.7*0.2/(-6.0)-x-x)+0.4*y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)-(-p)*q-5.1+(-p))-(-3.4)*((-0.6))-((-q)/(-2.3)-(-4.6))*(1.7*0.2/(-6.0)-p-p)+0.4*q)
            z2 <== (((-x)-(-x)*y-5.1+(-x))-(-3.4)*((-0.6))-((-y)/(-2.3)-(-4.6))*(1.7*0.2/(-6.0)-x-x)+0.4*y)
            wr [I 291; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 292
        !"test292"
        //let z0 = y
        //printfn "%d" <| 292
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 292; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 293
        !"test293"
        //let z0 = (-2.4)
        //printfn "%d" <| 293
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 294
        !"test294"
        //let z0 = y
        //printfn "%d" <| 294
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 294; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 295
        !"test295"
        //let z0 = 8.6
        //printfn "%d" <| 295
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 296
        !"test296"
        //let z0 = x
        //printfn "%d" <| 296
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 296; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 297
        !"test297"
        //let z0 = 5.8
        //printfn "%d" <| 297
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 298
        !"test298"
        //let z0 = (-y)
        //printfn "%d" <| 298
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 298; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 299
        !"test299"
        //let z0 = (-y)
        //printfn "%d" <| 299
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 299; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 300
        !"test300"
        //let z0 = x
        //printfn "%d" <| 300
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 300; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 301
        !"test301"
        //let z0 = (y-8.3/2.6+(-1.6)-(-1.5)-x+y/((-4.8)-(-x))+((-1.8)/(-y))*(-4.0))
        //printfn "%d" <| 301
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y-8.3/2.6+(-1.6)-(-1.5)-x+y/((-4.8)-(-x))+((-1.8)/(-y))*(-4.0))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q-8.3/2.6+(-1.6)-(-1.5)-p+q/((-4.8)-(-p))+((-1.8)/(-q))*(-4.0))
            z2 <== (y-8.3/2.6+(-1.6)-(-1.5)-x+y/((-4.8)-(-x))+((-1.8)/(-y))*(-4.0))
            wr [I 301; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 302
        !"test302"
        //let z0 = (8.4*7.0)
        //printfn "%d" <| 302
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 303
        !"test303"
        //let z0 = ((-y)-x+((-5.5)+(-0.2)*(-y))-(((-8.2))+8.6/3.6+4.0+y))
        //printfn "%d" <| 303
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)-x+((-5.5)+(-0.2)*(-y))-(((-8.2))+8.6/3.6+4.0+y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)-p+((-5.5)+(-0.2)*(-q))-(((-8.2))+8.6/3.6+4.0+q))
            z2 <== ((-y)-x+((-5.5)+(-0.2)*(-y))-(((-8.2))+8.6/3.6+4.0+y))
            wr [I 303; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 304
        !"test304"
        //let z0 = (-5.2)
        //printfn "%d" <| 304
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 305
        !"test305"
        //let z0 = (2.4*y+((-x)-((-x)*3.2-6.5-8.6*3.3)+((-y)+(-5.6)*(-y)-(-4.1))*(y))-(-6.4)-(-7.8)*((-7.2))*(x*4.0*x))
        //printfn "%d" <| 305
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((2.4*y+((-x)-((-x)*3.2-6.5-8.6*3.3)+((-y)+(-5.6)*(-y)-(-4.1))*(y))-(-6.4)-(-7.8)*((-7.2))*(x*4.0*x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (2.4*q+((-p)-((-p)*3.2-6.5-8.6*3.3)+((-q)+(-5.6)*(-q)-(-4.1))*(q))-(-6.4)-(-7.8)*((-7.2))*(p*4.0*p))
            z2 <== (2.4*y+((-x)-((-x)*3.2-6.5-8.6*3.3)+((-y)+(-5.6)*(-y)-(-4.1))*(y))-(-6.4)-(-7.8)*((-7.2))*(x*4.0*x))
            wr [I 305; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 306
        !"test306"
        //let z0 = ((8.0+(-y)/(y*(-5.5)-(-y)+3.3*(-x)))+0.3*x-x-(((-y)/8.2-(-x))*(-x)-(0.8-1.4+x+3.4)+((-x)/(-7.3))-x)*y)
        //printfn "%d" <| 306
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((8.0+(-y)/(y*(-5.5)-(-y)+3.3*(-x)))+0.3*x-x-(((-y)/8.2-(-x))*(-x)-(0.8-1.4+x+3.4)+((-x)/(-7.3))-x)*y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((8.0+(-q)/(q*(-5.5)-(-q)+3.3*(-p)))+0.3*p-p-(((-q)/8.2-(-p))*(-p)-(0.8-1.4+p+3.4)+((-p)/(-7.3))-p)*q)
            z2 <== ((8.0+(-y)/(y*(-5.5)-(-y)+3.3*(-x)))+0.3*x-x-(((-y)/8.2-(-x))*(-x)-(0.8-1.4+x+3.4)+((-x)/(-7.3))-x)*y)
            wr [I 306; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 307
        !"test307"
        //let z0 = ((x)+2.5+7.0/((-y)*y+(-6.3)))
        //printfn "%d" <| 307
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x)+2.5+7.0/((-y)*y+(-6.3)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p)+2.5+7.0/((-q)*q+(-6.3)))
            z2 <== ((x)+2.5+7.0/((-y)*y+(-6.3)))
            wr [I 307; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 308
        !"test308"
        //let z0 = ((-4.6)+(-7.3)/(5.2+((-x)*(-3.5)/(-0.4)-(-5.7))*y*(-x))+(-5.5)-(-x)/0.5+(-y)*(-4.8)*0.2)
        //printfn "%d" <| 308
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-4.6)+(-7.3)/(5.2+((-x)*(-3.5)/(-0.4)-(-5.7))*y*(-x))+(-5.5)-(-x)/0.5+(-y)*(-4.8)*0.2)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-4.6)+(-7.3)/(5.2+((-p)*(-3.5)/(-0.4)-(-5.7))*q*(-p))+(-5.5)-(-p)/0.5+(-q)*(-4.8)*0.2)
            z2 <== ((-4.6)+(-7.3)/(5.2+((-x)*(-3.5)/(-0.4)-(-5.7))*y*(-x))+(-5.5)-(-x)/0.5+(-y)*(-4.8)*0.2)
            wr [I 308; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 309
        !"test309"
        //let z0 = ((1.2-x+((-7.0)*(-y))/y)+y-(-1.6))
        //printfn "%d" <| 309
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((1.2-x+((-7.0)*(-y))/y)+y-(-1.6))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((1.2-p+((-7.0)*(-q))/q)+q-(-1.6))
            z2 <== ((1.2-x+((-7.0)*(-y))/y)+y-(-1.6))
            wr [I 309; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 310
        !"test310"
        //let z0 = (x-(-x))
        //printfn "%d" <| 310
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x-(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p-(-p))
            z2 <== (x-(-x))
            wr [I 310; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 311
        !"test311"
        //let z0 = 7.2
        //printfn "%d" <| 311
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 312
        !"test312"
        //let z0 = ((-x)*0.7*x+y)
        //printfn "%d" <| 312
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)*0.7*x+y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)*0.7*p+q)
            z2 <== ((-x)*0.7*x+y)
            wr [I 312; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 313
        !"test313"
        //let z0 = (1.0/((6.2)*4.0*8.6+((-x)-(-6.2)/y)-(x)))
        //printfn "%d" <| 313
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((1.0/((6.2)*4.0*8.6+((-x)-(-6.2)/y)-(x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (1.0/((6.2)*4.0*8.6+((-p)-(-6.2)/q)-(p)))
            z2 <== (1.0/((6.2)*4.0*8.6+((-x)-(-6.2)/y)-(x)))
            wr [I 313; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 314
        !"test314"
        //let z0 = ((((-y)*(-x)*(-x)+(-7.5))*3.0-(0.2/(-1.4)*(-y)*3.6-(-3.5)))-(y)-(y))
        //printfn "%d" <| 314
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-y)*(-x)*(-x)+(-7.5))*3.0-(0.2/(-1.4)*(-y)*3.6-(-3.5)))-(y)-(y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-q)*(-p)*(-p)+(-7.5))*3.0-(0.2/(-1.4)*(-q)*3.6-(-3.5)))-(q)-(q))
            z2 <== ((((-y)*(-x)*(-x)+(-7.5))*3.0-(0.2/(-1.4)*(-y)*3.6-(-3.5)))-(y)-(y))
            wr [I 314; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 315
        !"test315"
        //let z0 = ((-5.6)/((x*(-x))-(y)*x)/2.2)
        //printfn "%d" <| 315
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-5.6)/((x*(-x))-(y)*x)/2.2)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-5.6)/((p*(-p))-(q)*p)/2.2)
            z2 <== ((-5.6)/((x*(-x))-(y)*x)/2.2)
            wr [I 315; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 316
        !"test316"
        //let z0 = (-x)
        //printfn "%d" <| 316
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 316; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 317
        !"test317"
        //let z0 = (-8.0)
        //printfn "%d" <| 317
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 318
        !"test318"
        //let z0 = (-y)
        //printfn "%d" <| 318
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 318; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 319
        !"test319"
        //let z0 = (-x)
        //printfn "%d" <| 319
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 319; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 320
        !"test320"
        //let z0 = ((y-7.2+y+(-x)*((-x)+(-3.2)+(-6.2))-(x+(-4.0)+(-0.5)))*7.6+3.5*(-8.5)-2.0+(-5.2)*(4.6*5.2/(y/y)-(-6.5))-(((-x)+x+y)*(-y)))
        //printfn "%d" <| 320
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y-7.2+y+(-x)*((-x)+(-3.2)+(-6.2))-(x+(-4.0)+(-0.5)))*7.6+3.5*(-8.5)-2.0+(-5.2)*(4.6*5.2/(y/y)-(-6.5))-(((-x)+x+y)*(-y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q-7.2+q+(-p)*((-p)+(-3.2)+(-6.2))-(p+(-4.0)+(-0.5)))*7.6+3.5*(-8.5)-2.0+(-5.2)*(4.6*5.2/(q/q)-(-6.5))-(((-p)+p+q)*(-q)))
            z2 <== ((y-7.2+y+(-x)*((-x)+(-3.2)+(-6.2))-(x+(-4.0)+(-0.5)))*7.6+3.5*(-8.5)-2.0+(-5.2)*(4.6*5.2/(y/y)-(-6.5))-(((-x)+x+y)*(-y)))
            wr [I 320; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 321
        !"test321"
        //let z0 = (-0.3)
        //printfn "%d" <| 321
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 322
        !"test322"
        //let z0 = (x+(1.6)-(-5.6)*(5.7/(-x)*(-x)+((-5.0)-(-x))))
        //printfn "%d" <| 322
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x+(1.6)-(-5.6)*(5.7/(-x)*(-x)+((-5.0)-(-x))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p+(1.6)-(-5.6)*(5.7/(-p)*(-p)+((-5.0)-(-p))))
            z2 <== (x+(1.6)-(-5.6)*(5.7/(-x)*(-x)+((-5.0)-(-x))))
            wr [I 322; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 323
        !"test323"
        //let z0 = (-x)
        //printfn "%d" <| 323
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 323; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 324
        !"test324"
        //let z0 = x
        //printfn "%d" <| 324
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 324; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 325
        !"test325"
        //let z0 = (((-x))+(-2.0)*y+(-y))
        //printfn "%d" <| 325
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x))+(-2.0)*y+(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p))+(-2.0)*q+(-q))
            z2 <== (((-x))+(-2.0)*y+(-y))
            wr [I 325; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 326
        !"test326"
        //let z0 = (-4.2)
        //printfn "%d" <| 326
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 327
        !"test327"
        //let z0 = (-6.6)
        //printfn "%d" <| 327
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 328
        !"test328"
        //let z0 = 1.3+3.8
        //printfn "%d" <| 328
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 329
        !"test329"
        //let z0 = ((-4.1)-(((-y)+(-x)/0.5)-((-x)*x*6.0/y)/(-x))/((8.0*(-6.0)+x+(-x)*(-4.8))/(-y)+((-x))))
        //printfn "%d" <| 329
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-4.1)-(((-y)+(-x)/0.5)-((-x)*x*6.0/y)/(-x))/((8.0*(-6.0)+x+(-x)*(-4.8))/(-y)+((-x))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-4.1)-(((-q)+(-p)/0.5)-((-p)*p*6.0/q)/(-p))/((8.0*(-6.0)+p+(-p)*(-4.8))/(-q)+((-p))))
            z2 <== ((-4.1)-(((-y)+(-x)/0.5)-((-x)*x*6.0/y)/(-x))/((8.0*(-6.0)+x+(-x)*(-4.8))/(-y)+((-x))))
            wr [I 329; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 330
        !"test330"
        //let z0 = x
        //printfn "%d" <| 330
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 330; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 331
        !"test331"
        //let z0 = (-x)
        //printfn "%d" <| 331
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 331; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 332
        !"test332"
        //let z0 = ((y+(x))+x-(y+(-x))*(-x)+y+(-1.8)-7.5-(-y)*(-x)/(-y)/(-x)/(-x)/1.3+x+(-y)/(((-1.7)-(-4.5))-(-3.8)*6.7))
        //printfn "%d" <| 332
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y+(x))+x-(y+(-x))*(-x)+y+(-1.8)-7.5-(-y)*(-x)/(-y)/(-x)/(-x)/1.3+x+(-y)/(((-1.7)-(-4.5))-(-3.8)*6.7))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q+(p))+p-(q+(-p))*(-p)+q+(-1.8)-7.5-(-q)*(-p)/(-q)/(-p)/(-p)/1.3+p+(-q)/(((-1.7)-(-4.5))-(-3.8)*6.7))
            z2 <== ((y+(x))+x-(y+(-x))*(-x)+y+(-1.8)-7.5-(-y)*(-x)/(-y)/(-x)/(-x)/1.3+x+(-y)/(((-1.7)-(-4.5))-(-3.8)*6.7))
            wr [I 332; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 333
        !"test333"
        //let z0 = ((-8.6)+((-5.4)/(-8.2)-y)-(7.3-3.3)-3.6)
        //printfn "%d" <| 333
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-8.6)+((-5.4)/(-8.2)-y)-(7.3-3.3)-3.6)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-8.6)+((-5.4)/(-8.2)-q)-(7.3-3.3)-3.6)
            z2 <== ((-8.6)+((-5.4)/(-8.2)-y)-(7.3-3.3)-3.6)
            wr [I 333; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 334
        !"test334"
        //let z0 = ((-8.2))
        //printfn "%d" <| 334
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 335
        !"test335"
        //let z0 = (-x)
        //printfn "%d" <| 335
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 335; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 336
        !"test336"
        //let z0 = y
        //printfn "%d" <| 336
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 336; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 337
        !"test337"
        //let z0 = 5.4
        //printfn "%d" <| 337
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 338
        !"test338"
        //let z0 = (((x/y*2.1*(-5.7)))/(8.8+4.3/(-x)*y*1.5)+(-5.0)*(-1.6)-x+x)
        //printfn "%d" <| 338
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((x/y*2.1*(-5.7)))/(8.8+4.3/(-x)*y*1.5)+(-5.0)*(-1.6)-x+x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((p/q*2.1*(-5.7)))/(8.8+4.3/(-p)*q*1.5)+(-5.0)*(-1.6)-p+p)
            z2 <== (((x/y*2.1*(-5.7)))/(8.8+4.3/(-x)*y*1.5)+(-5.0)*(-1.6)-x+x)
            wr [I 338; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 339
        !"test339"
        //let z0 = (-4.0)
        //printfn "%d" <| 339
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 340
        !"test340"
        //let z0 = (((-6.6)-y-x+(-1.8))-(-8.0)+x/(x/3.1))
        //printfn "%d" <| 340
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-6.6)-y-x+(-1.8))-(-8.0)+x/(x/3.1))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-6.6)-q-p+(-1.8))-(-8.0)+p/(p/3.1))
            z2 <== (((-6.6)-y-x+(-1.8))-(-8.0)+x/(x/3.1))
            wr [I 340; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 341
        !"test341"
        //let z0 = 3.4
        //printfn "%d" <| 341
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 342
        !"test342"
        //let z0 = (6.4-(x)+(-y)*(-y)*(-3.1)-y+((-6.6)-2.7+0.6+7.0*x)/(-y))
        //printfn "%d" <| 342
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((6.4-(x)+(-y)*(-y)*(-3.1)-y+((-6.6)-2.7+0.6+7.0*x)/(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (6.4-(p)+(-q)*(-q)*(-3.1)-q+((-6.6)-2.7+0.6+7.0*p)/(-q))
            z2 <== (6.4-(x)+(-y)*(-y)*(-3.1)-y+((-6.6)-2.7+0.6+7.0*x)/(-y))
            wr [I 342; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 343
        !"test343"
        //let z0 = x
        //printfn "%d" <| 343
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 343; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 344
        !"test344"
        //let z0 = (-2.5)
        //printfn "%d" <| 344
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 345
        !"test345"
        //let z0 = (-x)
        //printfn "%d" <| 345
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 345; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 346
        !"test346"
        //let z0 = (-x)
        //printfn "%d" <| 346
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 346; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 347
        !"test347"
        //let z0 = (x*(-8.3)+((8.7)+1.1/((-y)+y-(-y)-8.8/y)-2.0-((-0.6)-(-y)))/((-4.5)+(-4.5)-(-1.8)*(-x)-(-x)+(-y)))
        //printfn "%d" <| 347
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x*(-8.3)+((8.7)+1.1/((-y)+y-(-y)-8.8/y)-2.0-((-0.6)-(-y)))/((-4.5)+(-4.5)-(-1.8)*(-x)-(-x)+(-y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p*(-8.3)+((8.7)+1.1/((-q)+q-(-q)-8.8/q)-2.0-((-0.6)-(-q)))/((-4.5)+(-4.5)-(-1.8)*(-p)-(-p)+(-q)))
            z2 <== (x*(-8.3)+((8.7)+1.1/((-y)+y-(-y)-8.8/y)-2.0-((-0.6)-(-y)))/((-4.5)+(-4.5)-(-1.8)*(-x)-(-x)+(-y)))
            wr [I 347; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 348
        !"test348"
        //let z0 = (y*(6.0+(-8.6)-0.4+(-5.6))+(-x)-y/(-x)-7.6-(-5.3)*(((-x)*(-2.8)))+(-x)+x)
        //printfn "%d" <| 348
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y*(6.0+(-8.6)-0.4+(-5.6))+(-x)-y/(-x)-7.6-(-5.3)*(((-x)*(-2.8)))+(-x)+x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q*(6.0+(-8.6)-0.4+(-5.6))+(-p)-q/(-p)-7.6-(-5.3)*(((-p)*(-2.8)))+(-p)+p)
            z2 <== (y*(6.0+(-8.6)-0.4+(-5.6))+(-x)-y/(-x)-7.6-(-5.3)*(((-x)*(-2.8)))+(-x)+x)
            wr [I 348; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 349
        !"test349"
        //let z0 = (-x)+x-x-(-y)+(-x)*1.6+(-0.8)+(-2.8)
        //printfn "%d" <| 349
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)+x-x-(-y)+(-x)*1.6+(-0.8)+(-2.8)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)+p-p-(-q)+(-p)*1.6+(-0.8)+(-2.8)
            z2 <== (-x)+x-x-(-y)+(-x)*1.6+(-0.8)+(-2.8)
            wr [I 349; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 350
        !"test350"
        //let z0 = (1.2/(-3.7))
        //printfn "%d" <| 350
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 351
        !"test351"
        //let z0 = x
        //printfn "%d" <| 351
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 351; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 352
        !"test352"
        //let z0 = (y/x+((x/(-x)-(-x)/x/x)-4.6*(-x)+(-x))-(0.6+((-1.4)/(-x))/6.3-(3.8+(-x))/((-y)-y-y)))
        //printfn "%d" <| 352
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y/x+((x/(-x)-(-x)/x/x)-4.6*(-x)+(-x))-(0.6+((-1.4)/(-x))/6.3-(3.8+(-x))/((-y)-y-y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q/p+((p/(-p)-(-p)/p/p)-4.6*(-p)+(-p))-(0.6+((-1.4)/(-p))/6.3-(3.8+(-p))/((-q)-q-q)))
            z2 <== (y/x+((x/(-x)-(-x)/x/x)-4.6*(-x)+(-x))-(0.6+((-1.4)/(-x))/6.3-(3.8+(-x))/((-y)-y-y)))
            wr [I 352; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 353
        !"test353"
        //let z0 = x
        //printfn "%d" <| 353
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 353; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 354
        !"test354"
        //let z0 = (((3.5*(-5.8)-(-2.2)))/(-5.4)*x/(1.1/0.2)+(-7.3)/x-7.8)
        //printfn "%d" <| 354
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((3.5*(-5.8)-(-2.2)))/(-5.4)*x/(1.1/0.2)+(-7.3)/x-7.8)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((3.5*(-5.8)-(-2.2)))/(-5.4)*p/(1.1/0.2)+(-7.3)/p-7.8)
            z2 <== (((3.5*(-5.8)-(-2.2)))/(-5.4)*x/(1.1/0.2)+(-7.3)/x-7.8)
            wr [I 354; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 355
        !"test355"
        //let z0 = (-7.5)
        //printfn "%d" <| 355
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 356
        !"test356"
        //let z0 = (-y)
        //printfn "%d" <| 356
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 356; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 357
        !"test357"
        //let z0 = (((-1.0)*((-7.8)-(-x))+x/(-x)+7.2-(-8.5)*(-x)*(-y))/(3.4+(5.8/(-y)-(-4.2)/5.5)*y-8.7-(-8.2)-(-y)-x+((-x)+(-y)-(-5.3)/(-y)))*(x)-(y/7.5/(-7.6))/y)
        //printfn "%d" <| 357
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-1.0)*((-7.8)-(-x))+x/(-x)+7.2-(-8.5)*(-x)*(-y))/(3.4+(5.8/(-y)-(-4.2)/5.5)*y-8.7-(-8.2)-(-y)-x+((-x)+(-y)-(-5.3)/(-y)))*(x)-(y/7.5/(-7.6))/y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-1.0)*((-7.8)-(-p))+p/(-p)+7.2-(-8.5)*(-p)*(-q))/(3.4+(5.8/(-q)-(-4.2)/5.5)*q-8.7-(-8.2)-(-q)-p+((-p)+(-q)-(-5.3)/(-q)))*(p)-(q/7.5/(-7.6))/q)
            z2 <== (((-1.0)*((-7.8)-(-x))+x/(-x)+7.2-(-8.5)*(-x)*(-y))/(3.4+(5.8/(-y)-(-4.2)/5.5)*y-8.7-(-8.2)-(-y)-x+((-x)+(-y)-(-5.3)/(-y)))*(x)-(y/7.5/(-7.6))/y)
            wr [I 357; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 358
        !"test358"
        //let z0 = (-y)
        //printfn "%d" <| 358
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 358; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 359
        !"test359"
        //let z0 = x
        //printfn "%d" <| 359
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 359; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 360
        !"test360"
        //let z0 = (-6.3)
        //printfn "%d" <| 360
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 361
        !"test361"
        //let z0 = (6.0*y)
        //printfn "%d" <| 361
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((6.0*y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (6.0*q)
            z2 <== (6.0*y)
            wr [I 361; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 362
        !"test362"
        //let z0 = 8.5
        //printfn "%d" <| 362
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 363
        !"test363"
        //let z0 = (-4.8)
        //printfn "%d" <| 363
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 364
        !"test364"
        //let z0 = (8.7)
        //printfn "%d" <| 364
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 365
        !"test365"
        //let z0 = (((5.8*(-x)+4.6-(-8.7))-(-y)-(-y)*(-3.4))/(y-((-1.6)/x-(-y)+(-5.1))/7.5)*(-8.4))
        //printfn "%d" <| 365
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((5.8*(-x)+4.6-(-8.7))-(-y)-(-y)*(-3.4))/(y-((-1.6)/x-(-y)+(-5.1))/7.5)*(-8.4))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((5.8*(-p)+4.6-(-8.7))-(-q)-(-q)*(-3.4))/(q-((-1.6)/p-(-q)+(-5.1))/7.5)*(-8.4))
            z2 <== (((5.8*(-x)+4.6-(-8.7))-(-y)-(-y)*(-3.4))/(y-((-1.6)/x-(-y)+(-5.1))/7.5)*(-8.4))
            wr [I 365; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 366
        !"test366"
        //let z0 = ((-y)/(2.7*(-7.5)-((-y)*(-y)-1.7/(-6.1)-(-3.4))-(-3.7))*3.8)
        //printfn "%d" <| 366
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)/(2.7*(-7.5)-((-y)*(-y)-1.7/(-6.1)-(-3.4))-(-3.7))*3.8)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)/(2.7*(-7.5)-((-q)*(-q)-1.7/(-6.1)-(-3.4))-(-3.7))*3.8)
            z2 <== ((-y)/(2.7*(-7.5)-((-y)*(-y)-1.7/(-6.1)-(-3.4))-(-3.7))*3.8)
            wr [I 366; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 367
        !"test367"
        //let z0 = ((x+(x/(-3.3))+((-y)-(-x)/(-y)-y)*(7.3*(-x)))+3.4+(3.6))
        //printfn "%d" <| 367
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x+(x/(-3.3))+((-y)-(-x)/(-y)-y)*(7.3*(-x)))+3.4+(3.6))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p+(p/(-3.3))+((-q)-(-p)/(-q)-q)*(7.3*(-p)))+3.4+(3.6))
            z2 <== ((x+(x/(-3.3))+((-y)-(-x)/(-y)-y)*(7.3*(-x)))+3.4+(3.6))
            wr [I 367; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 368
        !"test368"
        //let z0 = 6.6
        //printfn "%d" <| 368
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 369
        !"test369"
        //let z0 = x
        //printfn "%d" <| 369
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 369; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 370
        !"test370"
        //let z0 = ((6.5-(-2.6)))-(x/(-6.7))-(-x)+((-x))
        //printfn "%d" <| 370
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((6.5-(-2.6)))-(x/(-6.7))-(-x)+((-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((6.5-(-2.6)))-(p/(-6.7))-(-p)+((-p))
            z2 <== ((6.5-(-2.6)))-(x/(-6.7))-(-x)+((-x))
            wr [I 370; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 371
        !"test371"
        //let z0 = (2.3+((x)-(6.4-(-2.4)*y*(-x)*(-x))/5.0+(-x))*(((-y)/(-1.1))+((-4.8))*1.2*y-(-y)+(-3.7))+((-y)+(-x)*(-8.7)))
        //printfn "%d" <| 371
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((2.3+((x)-(6.4-(-2.4)*y*(-x)*(-x))/5.0+(-x))*(((-y)/(-1.1))+((-4.8))*1.2*y-(-y)+(-3.7))+((-y)+(-x)*(-8.7)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (2.3+((p)-(6.4-(-2.4)*q*(-p)*(-p))/5.0+(-p))*(((-q)/(-1.1))+((-4.8))*1.2*q-(-q)+(-3.7))+((-q)+(-p)*(-8.7)))
            z2 <== (2.3+((x)-(6.4-(-2.4)*y*(-x)*(-x))/5.0+(-x))*(((-y)/(-1.1))+((-4.8))*1.2*y-(-y)+(-3.7))+((-y)+(-x)*(-8.7)))
            wr [I 371; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 372
        !"test372"
        //let z0 = (-y)
        //printfn "%d" <| 372
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 372; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 373
        !"test373"
        //let z0 = (-4.6)
        //printfn "%d" <| 373
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 374
        !"test374"
        //let z0 = x
        //printfn "%d" <| 374
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 374; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 375
        !"test375"
        //let z0 = 1.0
        //printfn "%d" <| 375
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 376
        !"test376"
        //let z0 = 0.6
        //printfn "%d" <| 376
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 377
        !"test377"
        //let z0 = x
        //printfn "%d" <| 377
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 377; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 378
        !"test378"
        //let z0 = (-1.4)
        //printfn "%d" <| 378
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 379
        !"test379"
        //let z0 = ((-1.0))
        //printfn "%d" <| 379
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 380
        !"test380"
        //let z0 = ((-8.8)/(-y)+(((-x)+7.0/x/(-7.0)-(-3.1)))-(x+y-(-8.0)/0.6-(1.7+y-y-(-y)/3.4)+(4.6*8.7-(-7.3))+((-x)*2.7-4.1-(-x)/y))-(-1.3)/y)
        //printfn "%d" <| 380
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-8.8)/(-y)+(((-x)+7.0/x/(-7.0)-(-3.1)))-(x+y-(-8.0)/0.6-(1.7+y-y-(-y)/3.4)+(4.6*8.7-(-7.3))+((-x)*2.7-4.1-(-x)/y))-(-1.3)/y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-8.8)/(-q)+(((-p)+7.0/p/(-7.0)-(-3.1)))-(p+q-(-8.0)/0.6-(1.7+q-q-(-q)/3.4)+(4.6*8.7-(-7.3))+((-p)*2.7-4.1-(-p)/q))-(-1.3)/q)
            z2 <== ((-8.8)/(-y)+(((-x)+7.0/x/(-7.0)-(-3.1)))-(x+y-(-8.0)/0.6-(1.7+y-y-(-y)/3.4)+(4.6*8.7-(-7.3))+((-x)*2.7-4.1-(-x)/y))-(-1.3)/y)
            wr [I 380; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 381
        !"test381"
        //let z0 = ((8.8-(-x))-y*(7.3/((-2.8))+(-1.2)*(-x))*(-1.0)/(-1.3))
        //printfn "%d" <| 381
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((8.8-(-x))-y*(7.3/((-2.8))+(-1.2)*(-x))*(-1.0)/(-1.3))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((8.8-(-p))-q*(7.3/((-2.8))+(-1.2)*(-p))*(-1.0)/(-1.3))
            z2 <== ((8.8-(-x))-y*(7.3/((-2.8))+(-1.2)*(-x))*(-1.0)/(-1.3))
            wr [I 381; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 382
        !"test382"
        //let z0 = ((x-(8.0-8.4*x)+(y*(-6.2)-(-8.2))/((-7.1)/4.7/y)-((-8.4)+y*2.5*(-y)))*(((-y)/y)/(-3.5)+((-7.0)))/(0.1/((-0.8)+(-8.1)-x)-0.7+y/((-4.0)*(-y)/5.7)))
        //printfn "%d" <| 382
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x-(8.0-8.4*x)+(y*(-6.2)-(-8.2))/((-7.1)/4.7/y)-((-8.4)+y*2.5*(-y)))*(((-y)/y)/(-3.5)+((-7.0)))/(0.1/((-0.8)+(-8.1)-x)-0.7+y/((-4.0)*(-y)/5.7)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p-(8.0-8.4*p)+(q*(-6.2)-(-8.2))/((-7.1)/4.7/q)-((-8.4)+q*2.5*(-q)))*(((-q)/q)/(-3.5)+((-7.0)))/(0.1/((-0.8)+(-8.1)-p)-0.7+q/((-4.0)*(-q)/5.7)))
            z2 <== ((x-(8.0-8.4*x)+(y*(-6.2)-(-8.2))/((-7.1)/4.7/y)-((-8.4)+y*2.5*(-y)))*(((-y)/y)/(-3.5)+((-7.0)))/(0.1/((-0.8)+(-8.1)-x)-0.7+y/((-4.0)*(-y)/5.7)))
            wr [I 382; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 383
        !"test383"
        //let z0 = ((-y))/(y-(-2.8))+(-8.4)+(-4.1)-5.5
        //printfn "%d" <| 383
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y))/(y-(-2.8))+(-8.4)+(-4.1)-5.5).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q))/(q-(-2.8))+(-8.4)+(-4.1)-5.5
            z2 <== ((-y))/(y-(-2.8))+(-8.4)+(-4.1)-5.5
            wr [I 383; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 384
        !"test384"
        //let z0 = (8.1-(-x)-5.8)
        //printfn "%d" <| 384
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((8.1-(-x)-5.8)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (8.1-(-p)-5.8)
            z2 <== (8.1-(-x)-5.8)
            wr [I 384; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 385
        !"test385"
        //let z0 = (-x)
        //printfn "%d" <| 385
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 385; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 386
        !"test386"
        //let z0 = ((-0.1)-(0.5)+x*((-x)*(0.1/6.8*7.8-(-x))/(2.8/1.1/(-0.1)/3.5)*4.3+x/(-y)))
        //printfn "%d" <| 386
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.1)-(0.5)+x*((-x)*(0.1/6.8*7.8-(-x))/(2.8/1.1/(-0.1)/3.5)*4.3+x/(-y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.1)-(0.5)+p*((-p)*(0.1/6.8*7.8-(-p))/(2.8/1.1/(-0.1)/3.5)*4.3+p/(-q)))
            z2 <== ((-0.1)-(0.5)+x*((-x)*(0.1/6.8*7.8-(-x))/(2.8/1.1/(-0.1)/3.5)*4.3+x/(-y)))
            wr [I 386; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 387
        !"test387"
        //let z0 = (y/(-x)+(5.0*(-5.8))-(4.8+(-y)/x/(-2.4)-y)*((-x)/x/(-y)+(-8.8))-(y*(-7.5)+y)+y*(-5.7)/(-8.8)-(-y)+(-6.1)*(8.2+(3.6)/((-x)*y)/((-7.3)/y-(-7.0)/6.7*(-y))-0.8))
        //printfn "%d" <| 387
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y/(-x)+(5.0*(-5.8))-(4.8+(-y)/x/(-2.4)-y)*((-x)/x/(-y)+(-8.8))-(y*(-7.5)+y)+y*(-5.7)/(-8.8)-(-y)+(-6.1)*(8.2+(3.6)/((-x)*y)/((-7.3)/y-(-7.0)/6.7*(-y))-0.8))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q/(-p)+(5.0*(-5.8))-(4.8+(-q)/p/(-2.4)-q)*((-p)/p/(-q)+(-8.8))-(q*(-7.5)+q)+q*(-5.7)/(-8.8)-(-q)+(-6.1)*(8.2+(3.6)/((-p)*q)/((-7.3)/q-(-7.0)/6.7*(-q))-0.8))
            z2 <== (y/(-x)+(5.0*(-5.8))-(4.8+(-y)/x/(-2.4)-y)*((-x)/x/(-y)+(-8.8))-(y*(-7.5)+y)+y*(-5.7)/(-8.8)-(-y)+(-6.1)*(8.2+(3.6)/((-x)*y)/((-7.3)/y-(-7.0)/6.7*(-y))-0.8))
            wr [I 387; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 388
        !"test388"
        //let z0 = (((-x)/(-7.7)-(-6.0)/(y)-(-1.0)/0.0)*(-3.6)+((-x)+y/6.3/(-y)-y)-x-(-2.0)+((5.4)+((-y)-(-1.5))/y)/4.5-(-x)/((-5.8)+y+8.0))
        //printfn "%d" <| 388
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)/(-7.7)-(-6.0)/(y)-(-1.0)/0.0)*(-3.6)+((-x)+y/6.3/(-y)-y)-x-(-2.0)+((5.4)+((-y)-(-1.5))/y)/4.5-(-x)/((-5.8)+y+8.0))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)/(-7.7)-(-6.0)/(q)-(-1.0)/0.0)*(-3.6)+((-p)+q/6.3/(-q)-q)-p-(-2.0)+((5.4)+((-q)-(-1.5))/q)/4.5-(-p)/((-5.8)+q+8.0))
            z2 <== (((-x)/(-7.7)-(-6.0)/(y)-(-1.0)/0.0)*(-3.6)+((-x)+y/6.3/(-y)-y)-x-(-2.0)+((5.4)+((-y)-(-1.5))/y)/4.5-(-x)/((-5.8)+y+8.0))
            wr [I 388; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 389
        !"test389"
        //let z0 = ((((-y)-(-y)*(-y))-(7.7*(-0.2)+3.8/y))/(-x))
        //printfn "%d" <| 389
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-y)-(-y)*(-y))-(7.7*(-0.2)+3.8/y))/(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-q)-(-q)*(-q))-(7.7*(-0.2)+3.8/q))/(-p))
            z2 <== ((((-y)-(-y)*(-y))-(7.7*(-0.2)+3.8/y))/(-x))
            wr [I 389; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 390
        !"test390"
        //let z0 = 8.2
        //printfn "%d" <| 390
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 391
        !"test391"
        //let z0 = (y)
        //printfn "%d" <| 391
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q)
            z2 <== (y)
            wr [I 391; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 392
        !"test392"
        //let z0 = (5.1*((-x)+(-y))/((-6.8)*y)+6.1/y)
        //printfn "%d" <| 392
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.1*((-x)+(-y))/((-6.8)*y)+6.1/y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.1*((-p)+(-q))/((-6.8)*q)+6.1/q)
            z2 <== (5.1*((-x)+(-y))/((-6.8)*y)+6.1/y)
            wr [I 392; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 393
        !"test393"
        //let z0 = (-y)
        //printfn "%d" <| 393
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 393; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 394
        !"test394"
        //let z0 = ((-7.8)/((-5.6)*(x-(-2.0)*(-8.4)+(-2.0))/(-1.8)/(-y)+(-0.0))/(-4.8)*x*(-0.5)-((-y))-4.7-3.4/(-x))
        //printfn "%d" <| 394
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.8)/((-5.6)*(x-(-2.0)*(-8.4)+(-2.0))/(-1.8)/(-y)+(-0.0))/(-4.8)*x*(-0.5)-((-y))-4.7-3.4/(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.8)/((-5.6)*(p-(-2.0)*(-8.4)+(-2.0))/(-1.8)/(-q)+(-0.0))/(-4.8)*p*(-0.5)-((-q))-4.7-3.4/(-p))
            z2 <== ((-7.8)/((-5.6)*(x-(-2.0)*(-8.4)+(-2.0))/(-1.8)/(-y)+(-0.0))/(-4.8)*x*(-0.5)-((-y))-4.7-3.4/(-x))
            wr [I 394; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 395
        !"test395"
        //let z0 = (-x)
        //printfn "%d" <| 395
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 395; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 396
        !"test396"
        //let z0 = ((((-0.2)/(-y)*y/x)/7.0+x-5.5-y/(-6.2)/x))
        //printfn "%d" <| 396
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-0.2)/(-y)*y/x)/7.0+x-5.5-y/(-6.2)/x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-0.2)/(-q)*q/p)/7.0+p-5.5-q/(-6.2)/p))
            z2 <== ((((-0.2)/(-y)*y/x)/7.0+x-5.5-y/(-6.2)/x))
            wr [I 396; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 397
        !"test397"
        //let z0 = (x/(-3.7)/(-x)-(-8.1)-(-x))
        //printfn "%d" <| 397
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x/(-3.7)/(-x)-(-8.1)-(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p/(-3.7)/(-p)-(-8.1)-(-p))
            z2 <== (x/(-3.7)/(-x)-(-8.1)-(-x))
            wr [I 397; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 398
        !"test398"
        //let z0 = 8.5
        //printfn "%d" <| 398
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 399
        !"test399"
        //let z0 = (-6.2)
        //printfn "%d" <| 399
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 400
        !"test400"
        //let z0 = (-2.8)
        //printfn "%d" <| 400
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 401
        !"test401"
        //let z0 = ((-7.2))/(-y)/y/(8.0+((-7.3)))+(y-(8.4*(-6.7))*y+(7.7)*((-8.7)*y))
        //printfn "%d" <| 401
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.2))/(-y)/y/(8.0+((-7.3)))+(y-(8.4*(-6.7))*y+(7.7)*((-8.7)*y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.2))/(-q)/q/(8.0+((-7.3)))+(q-(8.4*(-6.7))*q+(7.7)*((-8.7)*q))
            z2 <== ((-7.2))/(-y)/y/(8.0+((-7.3)))+(y-(8.4*(-6.7))*y+(7.7)*((-8.7)*y))
            wr [I 401; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 402
        !"test402"
        //let z0 = 6.6
        //printfn "%d" <| 402
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 403
        !"test403"
        //let z0 = y
        //printfn "%d" <| 403
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 403; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 404
        !"test404"
        //let z0 = 0.3
        //printfn "%d" <| 404
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 405
        !"test405"
        //let z0 = (-2.3)
        //printfn "%d" <| 405
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 406
        !"test406"
        //let z0 = (-4.5)
        //printfn "%d" <| 406
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 407
        !"test407"
        //let z0 = (-x)
        //printfn "%d" <| 407
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 407; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 408
        !"test408"
        //let z0 = ((x+(-3.4)*8.5*(-4.6)))
        //printfn "%d" <| 408
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x+(-3.4)*8.5*(-4.6)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p+(-3.4)*8.5*(-4.6)))
            z2 <== ((x+(-3.4)*8.5*(-4.6)))
            wr [I 408; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 409
        !"test409"
        //let z0 = ((-4.3))
        //printfn "%d" <| 409
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 410
        !"test410"
        //let z0 = (-y)
        //printfn "%d" <| 410
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 410; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 411
        !"test411"
        //let z0 = 3.6
        //printfn "%d" <| 411
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 412
        !"test412"
        //let z0 = ((-y)*6.1/(-1.5))
        //printfn "%d" <| 412
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)*6.1/(-1.5))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)*6.1/(-1.5))
            z2 <== ((-y)*6.1/(-1.5))
            wr [I 412; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 413
        !"test413"
        //let z0 = x
        //printfn "%d" <| 413
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 413; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 414
        !"test414"
        //let z0 = (((y*6.8)/1.1)-(x)*y*((-7.2)+(-x))*(-y))
        //printfn "%d" <| 414
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((y*6.8)/1.1)-(x)*y*((-7.2)+(-x))*(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((q*6.8)/1.1)-(p)*q*((-7.2)+(-p))*(-q))
            z2 <== (((y*6.8)/1.1)-(x)*y*((-7.2)+(-x))*(-y))
            wr [I 414; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 415
        !"test415"
        //let z0 = (x+(-x)/(x-(-4.4)*(-x)/0.0-(x/(-0.3)+(-5.6)-(-2.8))))
        //printfn "%d" <| 415
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x+(-x)/(x-(-4.4)*(-x)/0.0-(x/(-0.3)+(-5.6)-(-2.8))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p+(-p)/(p-(-4.4)*(-p)/0.0-(p/(-0.3)+(-5.6)-(-2.8))))
            z2 <== (x+(-x)/(x-(-4.4)*(-x)/0.0-(x/(-0.3)+(-5.6)-(-2.8))))
            wr [I 415; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 416
        !"test416"
        //let z0 = ((-y)*y)
        //printfn "%d" <| 416
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)*y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)*q)
            z2 <== ((-y)*y)
            wr [I 416; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 417
        !"test417"
        //let z0 = 2.1
        //printfn "%d" <| 417
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 418
        !"test418"
        //let z0 = (-x)
        //printfn "%d" <| 418
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 418; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 419
        !"test419"
        //let z0 = 5.1
        //printfn "%d" <| 419
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 420
        !"test420"
        //let z0 = 3.6
        //printfn "%d" <| 420
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 421
        !"test421"
        //let z0 = x
        //printfn "%d" <| 421
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 421; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 422
        !"test422"
        //let z0 = (((-y)/x/7.2)/(-4.7)/((-x)*(-x)+(-x))*(-5.8)+(-y)*3.0-(-y)/x)
        //printfn "%d" <| 422
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)/x/7.2)/(-4.7)/((-x)*(-x)+(-x))*(-5.8)+(-y)*3.0-(-y)/x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)/p/7.2)/(-4.7)/((-p)*(-p)+(-p))*(-5.8)+(-q)*3.0-(-q)/p)
            z2 <== (((-y)/x/7.2)/(-4.7)/((-x)*(-x)+(-x))*(-5.8)+(-y)*3.0-(-y)/x)
            wr [I 422; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 423
        !"test423"
        //let z0 = 8.0
        //printfn "%d" <| 423
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 424
        !"test424"
        //let z0 = y
        //printfn "%d" <| 424
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 424; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 425
        !"test425"
        //let z0 = 4.6
        //printfn "%d" <| 425
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 426
        !"test426"
        //let z0 = ((7.7)+(y*(-1.3)*y*y)-3.6/((-1.6)*4.6+(-5.2))*x+(((-8.0)/7.8+(-y)-0.3)-((-x))*y-(6.7+(-1.4))*((-8.8))))
        //printfn "%d" <| 426
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((7.7)+(y*(-1.3)*y*y)-3.6/((-1.6)*4.6+(-5.2))*x+(((-8.0)/7.8+(-y)-0.3)-((-x))*y-(6.7+(-1.4))*((-8.8))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((7.7)+(q*(-1.3)*q*q)-3.6/((-1.6)*4.6+(-5.2))*p+(((-8.0)/7.8+(-q)-0.3)-((-p))*q-(6.7+(-1.4))*((-8.8))))
            z2 <== ((7.7)+(y*(-1.3)*y*y)-3.6/((-1.6)*4.6+(-5.2))*x+(((-8.0)/7.8+(-y)-0.3)-((-x))*y-(6.7+(-1.4))*((-8.8))))
            wr [I 426; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 427
        !"test427"
        //let z0 = x
        //printfn "%d" <| 427
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 427; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 428
        !"test428"
        //let z0 = (7.4)
        //printfn "%d" <| 428
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 429
        !"test429"
        //let z0 = (-x)
        //printfn "%d" <| 429
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 429; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 430
        !"test430"
        //let z0 = (8.3*(-x))
        //printfn "%d" <| 430
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((8.3*(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (8.3*(-p))
            z2 <== (8.3*(-x))
            wr [I 430; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 431
        !"test431"
        //let z0 = (-x)
        //printfn "%d" <| 431
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 431; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 432
        !"test432"
        //let z0 = (-7.5)
        //printfn "%d" <| 432
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 433
        !"test433"
        //let z0 = (y-(-x)-(-2.1)*(((-1.6)+y/5.7)/((-y)-7.8/(-y)-x)+6.8+(x/(-x)/1.5)))
        //printfn "%d" <| 433
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y-(-x)-(-2.1)*(((-1.6)+y/5.7)/((-y)-7.8/(-y)-x)+6.8+(x/(-x)/1.5)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q-(-p)-(-2.1)*(((-1.6)+q/5.7)/((-q)-7.8/(-q)-p)+6.8+(p/(-p)/1.5)))
            z2 <== (y-(-x)-(-2.1)*(((-1.6)+y/5.7)/((-y)-7.8/(-y)-x)+6.8+(x/(-x)/1.5)))
            wr [I 433; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 434
        !"test434"
        //let z0 = 0.6
        //printfn "%d" <| 434
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 435
        !"test435"
        //let z0 = (((-x)*(-3.7)-2.5/(-x))*((-y)+(-y)*(-x)*(-4.4)/(-5.1))-((-y)+0.2-6.8/(-6.1))/(x/1.3)-(5.1-6.7))+1.6/x+(-0.0)*y-0.3-(-x)
        //printfn "%d" <| 435
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)*(-3.7)-2.5/(-x))*((-y)+(-y)*(-x)*(-4.4)/(-5.1))-((-y)+0.2-6.8/(-6.1))/(x/1.3)-(5.1-6.7))+1.6/x+(-0.0)*y-0.3-(-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)*(-3.7)-2.5/(-p))*((-q)+(-q)*(-p)*(-4.4)/(-5.1))-((-q)+0.2-6.8/(-6.1))/(p/1.3)-(5.1-6.7))+1.6/p+(-0.0)*q-0.3-(-p)
            z2 <== (((-x)*(-3.7)-2.5/(-x))*((-y)+(-y)*(-x)*(-4.4)/(-5.1))-((-y)+0.2-6.8/(-6.1))/(x/1.3)-(5.1-6.7))+1.6/x+(-0.0)*y-0.3-(-x)
            wr [I 435; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 436
        !"test436"
        //let z0 = (((-4.6)-(-x))*(-y)-(-2.2)/y-(-x)-((y*5.8))/(-x)*y*(0.3*(-x)/6.8*(x*(-7.0)-(-5.4)-(-x)/8.5)-(-x)))
        //printfn "%d" <| 436
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-4.6)-(-x))*(-y)-(-2.2)/y-(-x)-((y*5.8))/(-x)*y*(0.3*(-x)/6.8*(x*(-7.0)-(-5.4)-(-x)/8.5)-(-x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-4.6)-(-p))*(-q)-(-2.2)/q-(-p)-((q*5.8))/(-p)*q*(0.3*(-p)/6.8*(p*(-7.0)-(-5.4)-(-p)/8.5)-(-p)))
            z2 <== (((-4.6)-(-x))*(-y)-(-2.2)/y-(-x)-((y*5.8))/(-x)*y*(0.3*(-x)/6.8*(x*(-7.0)-(-5.4)-(-x)/8.5)-(-x)))
            wr [I 436; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 437
        !"test437"
        //let z0 = (x-1.3+(5.7/4.3-y*(-1.5))-((-7.6)*(-5.5)+y*y)+(-6.6)*y)
        //printfn "%d" <| 437
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x-1.3+(5.7/4.3-y*(-1.5))-((-7.6)*(-5.5)+y*y)+(-6.6)*y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p-1.3+(5.7/4.3-q*(-1.5))-((-7.6)*(-5.5)+q*q)+(-6.6)*q)
            z2 <== (x-1.3+(5.7/4.3-y*(-1.5))-((-7.6)*(-5.5)+y*y)+(-6.6)*y)
            wr [I 437; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 438
        !"test438"
        //let z0 = ((-x))
        //printfn "%d" <| 438
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p))
            z2 <== ((-x))
            wr [I 438; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 439
        !"test439"
        //let z0 = ((-7.4)/(4.8*(-x)+(-7.6)+(-2.5))*((-y)*4.4/(-8.3)-x+((-y)*(-x)/x)/(y/1.3+y))*1.0-2.8)
        //printfn "%d" <| 439
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.4)/(4.8*(-x)+(-7.6)+(-2.5))*((-y)*4.4/(-8.3)-x+((-y)*(-x)/x)/(y/1.3+y))*1.0-2.8)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.4)/(4.8*(-p)+(-7.6)+(-2.5))*((-q)*4.4/(-8.3)-p+((-q)*(-p)/p)/(q/1.3+q))*1.0-2.8)
            z2 <== ((-7.4)/(4.8*(-x)+(-7.6)+(-2.5))*((-y)*4.4/(-8.3)-x+((-y)*(-x)/x)/(y/1.3+y))*1.0-2.8)
            wr [I 439; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 440
        !"test440"
        //let z0 = (((-3.7))+y*2.8)
        //printfn "%d" <| 440
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-3.7))+y*2.8)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-3.7))+q*2.8)
            z2 <== (((-3.7))+y*2.8)
            wr [I 440; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 441
        !"test441"
        //let z0 = (-x)
        //printfn "%d" <| 441
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 441; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 442
        !"test442"
        //let z0 = 3.2
        //printfn "%d" <| 442
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 443
        !"test443"
        //let z0 = y
        //printfn "%d" <| 443
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 443; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 444
        !"test444"
        //let z0 = (-4.2)-((-5.4)/2.6+(-y))+(x)-y/(y+(-y)-(-y)+5.8+(-6.4))-((x)/(x)/(x/y-(-x)))*1.4
        //printfn "%d" <| 444
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-4.2)-((-5.4)/2.6+(-y))+(x)-y/(y+(-y)-(-y)+5.8+(-6.4))-((x)/(x)/(x/y-(-x)))*1.4).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-4.2)-((-5.4)/2.6+(-q))+(p)-q/(q+(-q)-(-q)+5.8+(-6.4))-((p)/(p)/(p/q-(-p)))*1.4
            z2 <== (-4.2)-((-5.4)/2.6+(-y))+(x)-y/(y+(-y)-(-y)+5.8+(-6.4))-((x)/(x)/(x/y-(-x)))*1.4
            wr [I 444; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 445
        !"test445"
        //let z0 = x
        //printfn "%d" <| 445
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 445; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 446
        !"test446"
        //let z0 = (-y)
        //printfn "%d" <| 446
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 446; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 447
        !"test447"
        //let z0 = (-x)
        //printfn "%d" <| 447
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 447; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 448
        !"test448"
        //let z0 = (((-2.7)*x*x/x))-(-x)+(0.3)
        //printfn "%d" <| 448
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-2.7)*x*x/x))-(-x)+(0.3)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-2.7)*p*p/p))-(-p)+(0.3)
            z2 <== (((-2.7)*x*x/x))-(-x)+(0.3)
            wr [I 448; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 449
        !"test449"
        //let z0 = x
        //printfn "%d" <| 449
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 449; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 450
        !"test450"
        //let z0 = ((1.6*2.0-x*y/((-x)-y))/y/(-y))
        //printfn "%d" <| 450
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((1.6*2.0-x*y/((-x)-y))/y/(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((1.6*2.0-p*q/((-p)-q))/q/(-q))
            z2 <== ((1.6*2.0-x*y/((-x)-y))/y/(-y))
            wr [I 450; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 451
        !"test451"
        //let z0 = 1.5
        //printfn "%d" <| 451
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 452
        !"test452"
        //let z0 = ((-x)/2.2*(-x)/(-3.3)-x/(-7.5)+(-0.2)+x*((-6.3)+(-x)-(-y)/y*(-5.4)))
        //printfn "%d" <| 452
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)/2.2*(-x)/(-3.3)-x/(-7.5)+(-0.2)+x*((-6.3)+(-x)-(-y)/y*(-5.4)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)/2.2*(-p)/(-3.3)-p/(-7.5)+(-0.2)+p*((-6.3)+(-p)-(-q)/q*(-5.4)))
            z2 <== ((-x)/2.2*(-x)/(-3.3)-x/(-7.5)+(-0.2)+x*((-6.3)+(-x)-(-y)/y*(-5.4)))
            wr [I 452; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 453
        !"test453"
        //let z0 = (-2.5)
        //printfn "%d" <| 453
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 454
        !"test454"
        //let z0 = (-0.1)
        //printfn "%d" <| 454
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 455
        !"test455"
        //let z0 = y
        //printfn "%d" <| 455
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 455; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 456
        !"test456"
        //let z0 = (-7.1)
        //printfn "%d" <| 456
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 457
        !"test457"
        //let z0 = 6.3
        //printfn "%d" <| 457
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 458
        !"test458"
        //let z0 = 8.2
        //printfn "%d" <| 458
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 459
        !"test459"
        //let z0 = (4.1)
        //printfn "%d" <| 459
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 460
        !"test460"
        //let z0 = ((-x)+(-3.6)/2.6)
        //printfn "%d" <| 460
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+(-3.6)/2.6)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+(-3.6)/2.6)
            z2 <== ((-x)+(-3.6)/2.6)
            wr [I 460; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 461
        !"test461"
        //let z0 = ((-5.8))
        //printfn "%d" <| 461
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 462
        !"test462"
        //let z0 = ((((-8.8)+x*(-x)/y)*(y-(-4.7)/x)/(-5.8)/(-0.0)+(-x)-(-1.7)/0.3-(-7.2))-y+(((-x)*7.2)))
        //printfn "%d" <| 462
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-8.8)+x*(-x)/y)*(y-(-4.7)/x)/(-5.8)/(-0.0)+(-x)-(-1.7)/0.3-(-7.2))-y+(((-x)*7.2)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-8.8)+p*(-p)/q)*(q-(-4.7)/p)/(-5.8)/(-0.0)+(-p)-(-1.7)/0.3-(-7.2))-q+(((-p)*7.2)))
            z2 <== ((((-8.8)+x*(-x)/y)*(y-(-4.7)/x)/(-5.8)/(-0.0)+(-x)-(-1.7)/0.3-(-7.2))-y+(((-x)*7.2)))
            wr [I 462; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 463
        !"test463"
        //let z0 = 4.2
        //printfn "%d" <| 463
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 464
        !"test464"
        //let z0 = (-x)
        //printfn "%d" <| 464
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 464; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 465
        !"test465"
        //let z0 = ((-x)+((7.5*x/y/(-1.2)/(-x))-y-(-x)*6.1+3.1*y/(4.5)*(3.8-(-y)+0.1*x*y))*5.3*(-8.6))
        //printfn "%d" <| 465
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+((7.5*x/y/(-1.2)/(-x))-y-(-x)*6.1+3.1*y/(4.5)*(3.8-(-y)+0.1*x*y))*5.3*(-8.6))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+((7.5*p/q/(-1.2)/(-p))-q-(-p)*6.1+3.1*q/(4.5)*(3.8-(-q)+0.1*p*q))*5.3*(-8.6))
            z2 <== ((-x)+((7.5*x/y/(-1.2)/(-x))-y-(-x)*6.1+3.1*y/(4.5)*(3.8-(-y)+0.1*x*y))*5.3*(-8.6))
            wr [I 465; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 466
        !"test466"
        //let z0 = (1.3)
        //printfn "%d" <| 466
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 467
        !"test467"
        //let z0 = ((-3.6)-(-y)*((-x))/(5.5)-(7.7*(-8.7))/(-1.3)*6.4/((-1.7)*x)*(-4.3))
        //printfn "%d" <| 467
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-3.6)-(-y)*((-x))/(5.5)-(7.7*(-8.7))/(-1.3)*6.4/((-1.7)*x)*(-4.3))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-3.6)-(-q)*((-p))/(5.5)-(7.7*(-8.7))/(-1.3)*6.4/((-1.7)*p)*(-4.3))
            z2 <== ((-3.6)-(-y)*((-x))/(5.5)-(7.7*(-8.7))/(-1.3)*6.4/((-1.7)*x)*(-4.3))
            wr [I 467; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 468
        !"test468"
        //let z0 = ((-5.6)+(-5.4)/1.4/(-y))
        //printfn "%d" <| 468
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-5.6)+(-5.4)/1.4/(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-5.6)+(-5.4)/1.4/(-q))
            z2 <== ((-5.6)+(-5.4)/1.4/(-y))
            wr [I 468; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 469
        !"test469"
        //let z0 = 4.3
        //printfn "%d" <| 469
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 470
        !"test470"
        //let z0 = ((-7.3)*(y-(-2.8)/4.0/3.2*y)/(-5.1)*((-8.7)/(-8.7)+y*(-x)))
        //printfn "%d" <| 470
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.3)*(y-(-2.8)/4.0/3.2*y)/(-5.1)*((-8.7)/(-8.7)+y*(-x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.3)*(q-(-2.8)/4.0/3.2*q)/(-5.1)*((-8.7)/(-8.7)+q*(-p)))
            z2 <== ((-7.3)*(y-(-2.8)/4.0/3.2*y)/(-5.1)*((-8.7)/(-8.7)+y*(-x)))
            wr [I 470; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 471
        !"test471"
        //let z0 = (-y)
        //printfn "%d" <| 471
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 471; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 472
        !"test472"
        //let z0 = 8.0
        //printfn "%d" <| 472
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 473
        !"test473"
        //let z0 = (-6.5)
        //printfn "%d" <| 473
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 474
        !"test474"
        //let z0 = (-x)
        //printfn "%d" <| 474
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 474; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 475
        !"test475"
        //let z0 = ((-x)-y/(((-x)-4.5*8.5+x)/(-4.1)-(x-(-2.5)/3.1)-((-y)+y+4.5+(-x))*(0.3)))
        //printfn "%d" <| 475
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)-y/(((-x)-4.5*8.5+x)/(-4.1)-(x-(-2.5)/3.1)-((-y)+y+4.5+(-x))*(0.3)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)-q/(((-p)-4.5*8.5+p)/(-4.1)-(p-(-2.5)/3.1)-((-q)+q+4.5+(-p))*(0.3)))
            z2 <== ((-x)-y/(((-x)-4.5*8.5+x)/(-4.1)-(x-(-2.5)/3.1)-((-y)+y+4.5+(-x))*(0.3)))
            wr [I 475; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 476
        !"test476"
        //let z0 = ((-7.7)-(-7.4))
        //printfn "%d" <| 476
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 477
        !"test477"
        //let z0 = (-y)
        //printfn "%d" <| 477
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 477; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 478
        !"test478"
        //let z0 = (-x)
        //printfn "%d" <| 478
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 478; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 479
        !"test479"
        //let z0 = (((x+x+(-x))*((-y)-4.1-(-y))))
        //printfn "%d" <| 479
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((x+x+(-x))*((-y)-4.1-(-y))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((p+p+(-p))*((-q)-4.1-(-q))))
            z2 <== (((x+x+(-x))*((-y)-4.1-(-y))))
            wr [I 479; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 480
        !"test480"
        //let z0 = (((-x))-((-y)-y*x)-(x*(-x)))*(x)*y+((-8.7))
        //printfn "%d" <| 480
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x))-((-y)-y*x)-(x*(-x)))*(x)*y+((-8.7))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p))-((-q)-q*p)-(p*(-p)))*(p)*q+((-8.7))
            z2 <== (((-x))-((-y)-y*x)-(x*(-x)))*(x)*y+((-8.7))
            wr [I 480; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 481
        !"test481"
        //let z0 = x
        //printfn "%d" <| 481
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 481; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 482
        !"test482"
        //let z0 = ((((-y)+(-0.3)+(-y)*(-x)*8.3)/(-1.1)/(-x)-(-y)-4.3/(-x)*(3.7-1.6*x+y/(-x)))-x/(-0.4))
        //printfn "%d" <| 482
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-y)+(-0.3)+(-y)*(-x)*8.3)/(-1.1)/(-x)-(-y)-4.3/(-x)*(3.7-1.6*x+y/(-x)))-x/(-0.4))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-q)+(-0.3)+(-q)*(-p)*8.3)/(-1.1)/(-p)-(-q)-4.3/(-p)*(3.7-1.6*p+q/(-p)))-p/(-0.4))
            z2 <== ((((-y)+(-0.3)+(-y)*(-x)*8.3)/(-1.1)/(-x)-(-y)-4.3/(-x)*(3.7-1.6*x+y/(-x)))-x/(-0.4))
            wr [I 482; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 483
        !"test483"
        //let z0 = (x)
        //printfn "%d" <| 483
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p)
            z2 <== (x)
            wr [I 483; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 484
        !"test484"
        //let z0 = x
        //printfn "%d" <| 484
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 484; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 485
        !"test485"
        //let z0 = (-x)
        //printfn "%d" <| 485
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 485; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 486
        !"test486"
        //let z0 = ((-2.4)/(7.4)-((-x))+(-4.0)+(-x)-(-4.3)-(-5.6)/2.7)
        //printfn "%d" <| 486
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-2.4)/(7.4)-((-x))+(-4.0)+(-x)-(-4.3)-(-5.6)/2.7)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-2.4)/(7.4)-((-p))+(-4.0)+(-p)-(-4.3)-(-5.6)/2.7)
            z2 <== ((-2.4)/(7.4)-((-x))+(-4.0)+(-x)-(-4.3)-(-5.6)/2.7)
            wr [I 486; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 487
        !"test487"
        //let z0 = (((2.5)+(y+y*x+2.5))-((7.6)*7.7*(x/5.4+7.4/x))-1.0+((-x)+2.0)+x*y*(-x)-y-(-y)+((-3.5)-(-2.8)-1.2)+(-x))
        //printfn "%d" <| 487
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((2.5)+(y+y*x+2.5))-((7.6)*7.7*(x/5.4+7.4/x))-1.0+((-x)+2.0)+x*y*(-x)-y-(-y)+((-3.5)-(-2.8)-1.2)+(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((2.5)+(q+q*p+2.5))-((7.6)*7.7*(p/5.4+7.4/p))-1.0+((-p)+2.0)+p*q*(-p)-q-(-q)+((-3.5)-(-2.8)-1.2)+(-p))
            z2 <== (((2.5)+(y+y*x+2.5))-((7.6)*7.7*(x/5.4+7.4/x))-1.0+((-x)+2.0)+x*y*(-x)-y-(-y)+((-3.5)-(-2.8)-1.2)+(-x))
            wr [I 487; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 488
        !"test488"
        //let z0 = (((2.0/5.2*(-x))))
        //printfn "%d" <| 488
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((2.0/5.2*(-x))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((2.0/5.2*(-p))))
            z2 <== (((2.0/5.2*(-x))))
            wr [I 488; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 489
        !"test489"
        //let z0 = (x+(-6.4))+(2.7/0.3*(-8.4)+(-y)/8.0)+x/(7.7/1.6)-(x/(-y))
        //printfn "%d" <| 489
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x+(-6.4))+(2.7/0.3*(-8.4)+(-y)/8.0)+x/(7.7/1.6)-(x/(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p+(-6.4))+(2.7/0.3*(-8.4)+(-q)/8.0)+p/(7.7/1.6)-(p/(-q))
            z2 <== (x+(-6.4))+(2.7/0.3*(-8.4)+(-y)/8.0)+x/(7.7/1.6)-(x/(-y))
            wr [I 489; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 490
        !"test490"
        //let z0 = ((x*x/6.2/(5.5-(-x)*(-x)/4.1+3.5)+((-2.5)))/(x-(-x)-7.0-5.4/(-8.7)+y/8.2*(-5.8)/2.6-2.5)/((-x)*(-x)*0.5))
        //printfn "%d" <| 490
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x*x/6.2/(5.5-(-x)*(-x)/4.1+3.5)+((-2.5)))/(x-(-x)-7.0-5.4/(-8.7)+y/8.2*(-5.8)/2.6-2.5)/((-x)*(-x)*0.5))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p*p/6.2/(5.5-(-p)*(-p)/4.1+3.5)+((-2.5)))/(p-(-p)-7.0-5.4/(-8.7)+q/8.2*(-5.8)/2.6-2.5)/((-p)*(-p)*0.5))
            z2 <== ((x*x/6.2/(5.5-(-x)*(-x)/4.1+3.5)+((-2.5)))/(x-(-x)-7.0-5.4/(-8.7)+y/8.2*(-5.8)/2.6-2.5)/((-x)*(-x)*0.5))
            wr [I 490; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 491
        !"test491"
        //let z0 = 8.8
        //printfn "%d" <| 491
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 492
        !"test492"
        //let z0 = ((((-4.1)+(-y)+4.3)/((-5.8)+3.4+x)-4.3/x)/((5.8/(-4.7)*(-y)+(-x)-7.8))/3.1)
        //printfn "%d" <| 492
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-4.1)+(-y)+4.3)/((-5.8)+3.4+x)-4.3/x)/((5.8/(-4.7)*(-y)+(-x)-7.8))/3.1)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-4.1)+(-q)+4.3)/((-5.8)+3.4+p)-4.3/p)/((5.8/(-4.7)*(-q)+(-p)-7.8))/3.1)
            z2 <== ((((-4.1)+(-y)+4.3)/((-5.8)+3.4+x)-4.3/x)/((5.8/(-4.7)*(-y)+(-x)-7.8))/3.1)
            wr [I 492; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 493
        !"test493"
        //let z0 = (-y)
        //printfn "%d" <| 493
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 493; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 494
        !"test494"
        //let z0 = y
        //printfn "%d" <| 494
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 494; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 495
        !"test495"
        //let z0 = ((((-x)*x*y)/(-2.3)*((-0.8)+(-y)/(-x)/(-7.6))+(-x))*x*(x+((-x))-(6.2+x-(-0.1)/(-x))/(-6.2)-(7.1-(-7.1)*(-6.5)))+(-x)/(-4.2))
        //printfn "%d" <| 495
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-x)*x*y)/(-2.3)*((-0.8)+(-y)/(-x)/(-7.6))+(-x))*x*(x+((-x))-(6.2+x-(-0.1)/(-x))/(-6.2)-(7.1-(-7.1)*(-6.5)))+(-x)/(-4.2))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-p)*p*q)/(-2.3)*((-0.8)+(-q)/(-p)/(-7.6))+(-p))*p*(p+((-p))-(6.2+p-(-0.1)/(-p))/(-6.2)-(7.1-(-7.1)*(-6.5)))+(-p)/(-4.2))
            z2 <== ((((-x)*x*y)/(-2.3)*((-0.8)+(-y)/(-x)/(-7.6))+(-x))*x*(x+((-x))-(6.2+x-(-0.1)/(-x))/(-6.2)-(7.1-(-7.1)*(-6.5)))+(-x)/(-4.2))
            wr [I 495; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 496
        !"test496"
        //let z0 = (((x-y))+(-y)/y+2.4)
        //printfn "%d" <| 496
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((x-y))+(-y)/y+2.4)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((p-q))+(-q)/q+2.4)
            z2 <== (((x-y))+(-y)/y+2.4)
            wr [I 496; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 497
        !"test497"
        //let z0 = (-x)
        //printfn "%d" <| 497
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 497; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 498
        !"test498"
        //let z0 = y
        //printfn "%d" <| 498
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 498; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 499
        !"test499"
        //let z0 = 3.6
        //printfn "%d" <| 499
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 500
        !"test500"
        //let z0 = ((x-(-x)*(-0.4))/x)
        //printfn "%d" <| 500
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x-(-x)*(-0.4))/x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p-(-p)*(-0.4))/p)
            z2 <== ((x-(-x)*(-0.4))/x)
            wr [I 500; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 501
        !"test501"
        //let z0 = ((-2.3)-(-2.5)+(x)-1.2+(-3.3)/x)
        //printfn "%d" <| 501
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-2.3)-(-2.5)+(x)-1.2+(-3.3)/x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-2.3)-(-2.5)+(p)-1.2+(-3.3)/p)
            z2 <== ((-2.3)-(-2.5)+(x)-1.2+(-3.3)/x)
            wr [I 501; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 502
        !"test502"
        //let z0 = x
        //printfn "%d" <| 502
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 502; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 503
        !"test503"
        //let z0 = (((-x))/(0.2/(-5.2)+(-4.1)-(-x)*(-2.3))*5.6+((-0.6)*y)+((-y)+x-(-y))*((-x)/(6.0/(-8.1))*((-x)+(-y)*y+(-0.0)+(-7.7))-(-x)/(-8.8))-(-x))
        //printfn "%d" <| 503
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x))/(0.2/(-5.2)+(-4.1)-(-x)*(-2.3))*5.6+((-0.6)*y)+((-y)+x-(-y))*((-x)/(6.0/(-8.1))*((-x)+(-y)*y+(-0.0)+(-7.7))-(-x)/(-8.8))-(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p))/(0.2/(-5.2)+(-4.1)-(-p)*(-2.3))*5.6+((-0.6)*q)+((-q)+p-(-q))*((-p)/(6.0/(-8.1))*((-p)+(-q)*q+(-0.0)+(-7.7))-(-p)/(-8.8))-(-p))
            z2 <== (((-x))/(0.2/(-5.2)+(-4.1)-(-x)*(-2.3))*5.6+((-0.6)*y)+((-y)+x-(-y))*((-x)/(6.0/(-8.1))*((-x)+(-y)*y+(-0.0)+(-7.7))-(-x)/(-8.8))-(-x))
            wr [I 503; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 504
        !"test504"
        //let z0 = ((((-3.5))/(-y)+(y+(-y))+y))
        //printfn "%d" <| 504
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-3.5))/(-y)+(y+(-y))+y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-3.5))/(-q)+(q+(-q))+q))
            z2 <== ((((-3.5))/(-y)+(y+(-y))+y))
            wr [I 504; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 505
        !"test505"
        //let z0 = x
        //printfn "%d" <| 505
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 505; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 506
        !"test506"
        //let z0 = (-y)
        //printfn "%d" <| 506
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 506; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 507
        !"test507"
        //let z0 = (-x)
        //printfn "%d" <| 507
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 507; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 508
        !"test508"
        //let z0 = 1.8+(x/x*(-x)/y*(-4.1))+(-1.4)
        //printfn "%d" <| 508
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (1.8+(x/x*(-x)/y*(-4.1))+(-1.4)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 1.8+(p/p*(-p)/q*(-4.1))+(-1.4)
            z2 <== 1.8+(x/x*(-x)/y*(-4.1))+(-1.4)
            wr [I 508; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 509
        !"test509"
        //let z0 = x
        //printfn "%d" <| 509
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 509; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 510
        !"test510"
        //let z0 = ((-x)-(y/(-x)+(-8.5)*(-y)+(-2.6))-((-6.5))+x)-x/(-6.0)
        //printfn "%d" <| 510
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)-(y/(-x)+(-8.5)*(-y)+(-2.6))-((-6.5))+x)-x/(-6.0)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)-(q/(-p)+(-8.5)*(-q)+(-2.6))-((-6.5))+p)-p/(-6.0)
            z2 <== ((-x)-(y/(-x)+(-8.5)*(-y)+(-2.6))-((-6.5))+x)-x/(-6.0)
            wr [I 510; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 511
        !"test511"
        //let z0 = ((-x)-(-y)/y)
        //printfn "%d" <| 511
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)-(-y)/y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)-(-q)/q)
            z2 <== ((-x)-(-y)/y)
            wr [I 511; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 512
        !"test512"
        //let z0 = (((y-y*y)/(-x))*((-1.2)+6.2/((-x)+(-x))+(-x)*6.7)*(-1.4))
        //printfn "%d" <| 512
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((y-y*y)/(-x))*((-1.2)+6.2/((-x)+(-x))+(-x)*6.7)*(-1.4))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((q-q*q)/(-p))*((-1.2)+6.2/((-p)+(-p))+(-p)*6.7)*(-1.4))
            z2 <== (((y-y*y)/(-x))*((-1.2)+6.2/((-x)+(-x))+(-x)*6.7)*(-1.4))
            wr [I 512; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 513
        !"test513"
        //let z0 = (-x)
        //printfn "%d" <| 513
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 513; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 514
        !"test514"
        //let z0 = 7.3
        //printfn "%d" <| 514
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 515
        !"test515"
        //let z0 = (-x)*(-x)*y/(((-6.7)/x*4.4-y)+2.2+0.8/((-8.4)/(-6.2)/4.8))-(-0.4)
        //printfn "%d" <| 515
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)*(-x)*y/(((-6.7)/x*4.4-y)+2.2+0.8/((-8.4)/(-6.2)/4.8))-(-0.4)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)*(-p)*q/(((-6.7)/p*4.4-q)+2.2+0.8/((-8.4)/(-6.2)/4.8))-(-0.4)
            z2 <== (-x)*(-x)*y/(((-6.7)/x*4.4-y)+2.2+0.8/((-8.4)/(-6.2)/4.8))-(-0.4)
            wr [I 515; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 516
        !"test516"
        //let z0 = (((5.8+8.4*8.0/(-4.5)))+(-x))
        //printfn "%d" <| 516
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((5.8+8.4*8.0/(-4.5)))+(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((5.8+8.4*8.0/(-4.5)))+(-p))
            z2 <== (((5.8+8.4*8.0/(-4.5)))+(-x))
            wr [I 516; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 517
        !"test517"
        //let z0 = ((-0.7)+x-(x-(y+y+(-7.4)*(-5.7))))
        //printfn "%d" <| 517
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.7)+x-(x-(y+y+(-7.4)*(-5.7))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.7)+p-(p-(q+q+(-7.4)*(-5.7))))
            z2 <== ((-0.7)+x-(x-(y+y+(-7.4)*(-5.7))))
            wr [I 517; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 518
        !"test518"
        //let z0 = 7.0
        //printfn "%d" <| 518
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 519
        !"test519"
        //let z0 = (-y)+(-0.0)/(-y)+(-0.7)*x
        //printfn "%d" <| 519
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)+(-0.0)/(-y)+(-0.7)*x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)+(-0.0)/(-q)+(-0.7)*p
            z2 <== (-y)+(-0.0)/(-y)+(-0.7)*x
            wr [I 519; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 520
        !"test520"
        //let z0 = ((5.4/(-8.6)-(-y)+(-y)-(-x))/(x*7.6*y-(-7.6)*(-y)*(2.0+(-y))/3.1)-(y-x*((-8.8)*y)+7.3)/(-0.7))
        //printfn "%d" <| 520
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((5.4/(-8.6)-(-y)+(-y)-(-x))/(x*7.6*y-(-7.6)*(-y)*(2.0+(-y))/3.1)-(y-x*((-8.8)*y)+7.3)/(-0.7))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((5.4/(-8.6)-(-q)+(-q)-(-p))/(p*7.6*q-(-7.6)*(-q)*(2.0+(-q))/3.1)-(q-p*((-8.8)*q)+7.3)/(-0.7))
            z2 <== ((5.4/(-8.6)-(-y)+(-y)-(-x))/(x*7.6*y-(-7.6)*(-y)*(2.0+(-y))/3.1)-(y-x*((-8.8)*y)+7.3)/(-0.7))
            wr [I 520; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 521
        !"test521"
        //let z0 = (-y)
        //printfn "%d" <| 521
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 521; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 522
        !"test522"
        //let z0 = x
        //printfn "%d" <| 522
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 522; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 523
        !"test523"
        //let z0 = ((-5.1)/8.2+(x*0.4*y)/7.2)
        //printfn "%d" <| 523
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-5.1)/8.2+(x*0.4*y)/7.2)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-5.1)/8.2+(p*0.4*q)/7.2)
            z2 <== ((-5.1)/8.2+(x*0.4*y)/7.2)
            wr [I 523; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 524
        !"test524"
        //let z0 = 8.8
        //printfn "%d" <| 524
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 525
        !"test525"
        //let z0 = (-0.1)
        //printfn "%d" <| 525
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 526
        !"test526"
        //let z0 = (-y)
        //printfn "%d" <| 526
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 526; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 527
        !"test527"
        //let z0 = (-x)
        //printfn "%d" <| 527
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 527; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 528
        !"test528"
        //let z0 = 4.2
        //printfn "%d" <| 528
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 529
        !"test529"
        //let z0 = 7.1
        //printfn "%d" <| 529
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 530
        !"test530"
        //let z0 = (-8.3)
        //printfn "%d" <| 530
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 531
        !"test531"
        //let z0 = 6.8
        //printfn "%d" <| 531
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 532
        !"test532"
        //let z0 = (-5.4)
        //printfn "%d" <| 532
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 533
        !"test533"
        //let z0 = (x/(-y)+(-y)+x*7.0/(-8.3)+2.6-3.2)/(-y)/(-x)-((-5.3)-y)/x
        //printfn "%d" <| 533
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x/(-y)+(-y)+x*7.0/(-8.3)+2.6-3.2)/(-y)/(-x)-((-5.3)-y)/x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p/(-q)+(-q)+p*7.0/(-8.3)+2.6-3.2)/(-q)/(-p)-((-5.3)-q)/p
            z2 <== (x/(-y)+(-y)+x*7.0/(-8.3)+2.6-3.2)/(-y)/(-x)-((-5.3)-y)/x
            wr [I 533; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 534
        !"test534"
        //let z0 = (y/((-4.1)+(-1.8))/(-y)*((6.0-y-0.1)/(-y)/(-8.0)-7.0*(0.7-y+y))*((-y)))
        //printfn "%d" <| 534
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y/((-4.1)+(-1.8))/(-y)*((6.0-y-0.1)/(-y)/(-8.0)-7.0*(0.7-y+y))*((-y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q/((-4.1)+(-1.8))/(-q)*((6.0-q-0.1)/(-q)/(-8.0)-7.0*(0.7-q+q))*((-q)))
            z2 <== (y/((-4.1)+(-1.8))/(-y)*((6.0-y-0.1)/(-y)/(-8.0)-7.0*(0.7-y+y))*((-y)))
            wr [I 534; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 535
        !"test535"
        //let z0 = (-2.1)-(-0.5)*(-x)-1.6-(-y)/(-2.3)/1.4-(-0.8)+y/(y+x-x-x)
        //printfn "%d" <| 535
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-2.1)-(-0.5)*(-x)-1.6-(-y)/(-2.3)/1.4-(-0.8)+y/(y+x-x-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-2.1)-(-0.5)*(-p)-1.6-(-q)/(-2.3)/1.4-(-0.8)+q/(q+p-p-p)
            z2 <== (-2.1)-(-0.5)*(-x)-1.6-(-y)/(-2.3)/1.4-(-0.8)+y/(y+x-x-x)
            wr [I 535; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 536
        !"test536"
        //let z0 = 0.6
        //printfn "%d" <| 536
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 537
        !"test537"
        //let z0 = ((-3.4)*y-(-x)-((-x)/y*(-8.6)))+x/x
        //printfn "%d" <| 537
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-3.4)*y-(-x)-((-x)/y*(-8.6)))+x/x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-3.4)*q-(-p)-((-p)/q*(-8.6)))+p/p
            z2 <== ((-3.4)*y-(-x)-((-x)/y*(-8.6)))+x/x
            wr [I 537; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 538
        !"test538"
        //let z0 = ((4.0*(3.1/y*(-x)*(-1.6)/0.1)*(-2.0))-(-x)*6.2)
        //printfn "%d" <| 538
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((4.0*(3.1/y*(-x)*(-1.6)/0.1)*(-2.0))-(-x)*6.2)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((4.0*(3.1/q*(-p)*(-1.6)/0.1)*(-2.0))-(-p)*6.2)
            z2 <== ((4.0*(3.1/y*(-x)*(-1.6)/0.1)*(-2.0))-(-x)*6.2)
            wr [I 538; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 539
        !"test539"
        //let z0 = (y+2.6-(((-y)*(-3.5)-y-5.5-(-7.3))+(-y)*(-y)))
        //printfn "%d" <| 539
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+2.6-(((-y)*(-3.5)-y-5.5-(-7.3))+(-y)*(-y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+2.6-(((-q)*(-3.5)-q-5.5-(-7.3))+(-q)*(-q)))
            z2 <== (y+2.6-(((-y)*(-3.5)-y-5.5-(-7.3))+(-y)*(-y)))
            wr [I 539; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 540
        !"test540"
        //let z0 = (y*x+((5.8+(-y))/y)*y)
        //printfn "%d" <| 540
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y*x+((5.8+(-y))/y)*y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q*p+((5.8+(-q))/q)*q)
            z2 <== (y*x+((5.8+(-y))/y)*y)
            wr [I 540; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 541
        !"test541"
        //let z0 = y
        //printfn "%d" <| 541
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 541; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 542
        !"test542"
        //let z0 = 8.2
        //printfn "%d" <| 542
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 543
        !"test543"
        //let z0 = y
        //printfn "%d" <| 543
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 543; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 544
        !"test544"
        //let z0 = (0.7*((-x)*(-7.1)+0.7+(y+(-x)))+((-3.1)+(x+y/1.5/2.8)-((-x))/(-y)+((-8.4)/(-x)+(-5.2)-(-6.5)))-(y+5.8/y/(-2.7)*0.5/7.3*(-6.4)-(-6.3)*0.7))
        //printfn "%d" <| 544
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((0.7*((-x)*(-7.1)+0.7+(y+(-x)))+((-3.1)+(x+y/1.5/2.8)-((-x))/(-y)+((-8.4)/(-x)+(-5.2)-(-6.5)))-(y+5.8/y/(-2.7)*0.5/7.3*(-6.4)-(-6.3)*0.7))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (0.7*((-p)*(-7.1)+0.7+(q+(-p)))+((-3.1)+(p+q/1.5/2.8)-((-p))/(-q)+((-8.4)/(-p)+(-5.2)-(-6.5)))-(q+5.8/q/(-2.7)*0.5/7.3*(-6.4)-(-6.3)*0.7))
            z2 <== (0.7*((-x)*(-7.1)+0.7+(y+(-x)))+((-3.1)+(x+y/1.5/2.8)-((-x))/(-y)+((-8.4)/(-x)+(-5.2)-(-6.5)))-(y+5.8/y/(-2.7)*0.5/7.3*(-6.4)-(-6.3)*0.7))
            wr [I 544; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 545
        !"test545"
        //let z0 = x
        //printfn "%d" <| 545
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 545; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 546
        !"test546"
        //let z0 = ((3.8+y-y/(-y)-3.0))
        //printfn "%d" <| 546
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((3.8+y-y/(-y)-3.0))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((3.8+q-q/(-q)-3.0))
            z2 <== ((3.8+y-y/(-y)-3.0))
            wr [I 546; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 547
        !"test547"
        //let z0 = ((0.5-7.8*3.1-y)*(-x))
        //printfn "%d" <| 547
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((0.5-7.8*3.1-y)*(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((0.5-7.8*3.1-q)*(-p))
            z2 <== ((0.5-7.8*3.1-y)*(-x))
            wr [I 547; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 548
        !"test548"
        //let z0 = 1.4
        //printfn "%d" <| 548
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 549
        !"test549"
        //let z0 = (-y)
        //printfn "%d" <| 549
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 549; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 550
        !"test550"
        //let z0 = (7.1/((-2.2)*(-y)-x+y)-5.6/((-y)+(x/(-8.5)-0.2/3.3)))
        //printfn "%d" <| 550
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.1/((-2.2)*(-y)-x+y)-5.6/((-y)+(x/(-8.5)-0.2/3.3)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.1/((-2.2)*(-q)-p+q)-5.6/((-q)+(p/(-8.5)-0.2/3.3)))
            z2 <== (7.1/((-2.2)*(-y)-x+y)-5.6/((-y)+(x/(-8.5)-0.2/3.3)))
            wr [I 550; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 551
        !"test551"
        //let z0 = (-3.5)
        //printfn "%d" <| 551
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 552
        !"test552"
        //let z0 = 0.4
        //printfn "%d" <| 552
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 553
        !"test553"
        //let z0 = x
        //printfn "%d" <| 553
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 553; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 554
        !"test554"
        //let z0 = 5.8
        //printfn "%d" <| 554
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 555
        !"test555"
        //let z0 = 5.4
        //printfn "%d" <| 555
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 556
        !"test556"
        //let z0 = ((x+(-6.8)*(-7.1)-(-y))+(y*6.3+y*(-6.6)+(-y))-(1.2+(-4.3)*(-y)/(-7.7)/(y*x/(-x)+(-y)))*(-1.0))
        //printfn "%d" <| 556
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x+(-6.8)*(-7.1)-(-y))+(y*6.3+y*(-6.6)+(-y))-(1.2+(-4.3)*(-y)/(-7.7)/(y*x/(-x)+(-y)))*(-1.0))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p+(-6.8)*(-7.1)-(-q))+(q*6.3+q*(-6.6)+(-q))-(1.2+(-4.3)*(-q)/(-7.7)/(q*p/(-p)+(-q)))*(-1.0))
            z2 <== ((x+(-6.8)*(-7.1)-(-y))+(y*6.3+y*(-6.6)+(-y))-(1.2+(-4.3)*(-y)/(-7.7)/(y*x/(-x)+(-y)))*(-1.0))
            wr [I 556; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 557
        !"test557"
        //let z0 = (x)
        //printfn "%d" <| 557
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p)
            z2 <== (x)
            wr [I 557; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 558
        !"test558"
        //let z0 = (-0.1)
        //printfn "%d" <| 558
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 559
        !"test559"
        //let z0 = (7.0-y*(-0.6)+(((-x)*(-3.4))))
        //printfn "%d" <| 559
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.0-y*(-0.6)+(((-x)*(-3.4))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.0-q*(-0.6)+(((-p)*(-3.4))))
            z2 <== (7.0-y*(-0.6)+(((-x)*(-3.4))))
            wr [I 559; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 560
        !"test560"
        //let z0 = 8.5
        //printfn "%d" <| 560
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 561
        !"test561"
        //let z0 = (-3.7)
        //printfn "%d" <| 561
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 562
        !"test562"
        //let z0 = (-y)
        //printfn "%d" <| 562
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 562; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 563
        !"test563"
        //let z0 = y
        //printfn "%d" <| 563
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 563; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 564
        !"test564"
        //let z0 = (-y)
        //printfn "%d" <| 564
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 564; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 565
        !"test565"
        //let z0 = y
        //printfn "%d" <| 565
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 565; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 566
        !"test566"
        //let z0 = (-y)
        //printfn "%d" <| 566
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 566; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 567
        !"test567"
        //let z0 = (-4.1)+((-x)*(-5.5)-(-8.7)-x+(-x))+(-y)
        //printfn "%d" <| 567
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-4.1)+((-x)*(-5.5)-(-8.7)-x+(-x))+(-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-4.1)+((-p)*(-5.5)-(-8.7)-p+(-p))+(-q)
            z2 <== (-4.1)+((-x)*(-5.5)-(-8.7)-x+(-x))+(-y)
            wr [I 567; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 568
        !"test568"
        //let z0 = (8.2+(x*(-5.3)+2.6/(-x)/y)-(-x)/(2.3+x/x+3.1+(-0.0)+((-x)-(-5.3))-((-x)+(-x)+(-x)/(-x))/3.2)+((-x)/((-4.3)-0.6-(-2.8))/(-4.0)-(-3.5)*(-3.3)))
        //printfn "%d" <| 568
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((8.2+(x*(-5.3)+2.6/(-x)/y)-(-x)/(2.3+x/x+3.1+(-0.0)+((-x)-(-5.3))-((-x)+(-x)+(-x)/(-x))/3.2)+((-x)/((-4.3)-0.6-(-2.8))/(-4.0)-(-3.5)*(-3.3)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (8.2+(p*(-5.3)+2.6/(-p)/q)-(-p)/(2.3+p/p+3.1+(-0.0)+((-p)-(-5.3))-((-p)+(-p)+(-p)/(-p))/3.2)+((-p)/((-4.3)-0.6-(-2.8))/(-4.0)-(-3.5)*(-3.3)))
            z2 <== (8.2+(x*(-5.3)+2.6/(-x)/y)-(-x)/(2.3+x/x+3.1+(-0.0)+((-x)-(-5.3))-((-x)+(-x)+(-x)/(-x))/3.2)+((-x)/((-4.3)-0.6-(-2.8))/(-4.0)-(-3.5)*(-3.3)))
            wr [I 568; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 569
        !"test569"
        //let z0 = ((2.3)/((3.4*2.0/y)+(-6.6))+x/(((-2.8)-6.6-x)+y/x/(-2.4)))
        //printfn "%d" <| 569
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((2.3)/((3.4*2.0/y)+(-6.6))+x/(((-2.8)-6.6-x)+y/x/(-2.4)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((2.3)/((3.4*2.0/q)+(-6.6))+p/(((-2.8)-6.6-p)+q/p/(-2.4)))
            z2 <== ((2.3)/((3.4*2.0/y)+(-6.6))+x/(((-2.8)-6.6-x)+y/x/(-2.4)))
            wr [I 569; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 570
        !"test570"
        //let z0 = y
        //printfn "%d" <| 570
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 570; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 571
        !"test571"
        //let z0 = (5.8-(-y)/(y+(-y)+3.5+y)+(-y))
        //printfn "%d" <| 571
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.8-(-y)/(y+(-y)+3.5+y)+(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.8-(-q)/(q+(-q)+3.5+q)+(-q))
            z2 <== (5.8-(-y)/(y+(-y)+3.5+y)+(-y))
            wr [I 571; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 572
        !"test572"
        //let z0 = ((((-1.0)-(-3.2)-(-y))*y)+(-y)*7.5+((-y)))
        //printfn "%d" <| 572
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-1.0)-(-3.2)-(-y))*y)+(-y)*7.5+((-y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-1.0)-(-3.2)-(-q))*q)+(-q)*7.5+((-q)))
            z2 <== ((((-1.0)-(-3.2)-(-y))*y)+(-y)*7.5+((-y)))
            wr [I 572; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 573
        !"test573"
        //let z0 = (-x)*(((-y)*(-y)/4.5))*(-y)*(-4.0)*(-7.5)
        //printfn "%d" <| 573
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)*(((-y)*(-y)/4.5))*(-y)*(-4.0)*(-7.5)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)*(((-q)*(-q)/4.5))*(-q)*(-4.0)*(-7.5)
            z2 <== (-x)*(((-y)*(-y)/4.5))*(-y)*(-4.0)*(-7.5)
            wr [I 573; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 574
        !"test574"
        //let z0 = (y)
        //printfn "%d" <| 574
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q)
            z2 <== (y)
            wr [I 574; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 575
        !"test575"
        //let z0 = (((-6.1)+(-y)-x)+(1.8*y+(-x)-(-4.8)+(-7.7))/(0.5)+(y*2.3*(-5.3)-y+(-y))*8.1)
        //printfn "%d" <| 575
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-6.1)+(-y)-x)+(1.8*y+(-x)-(-4.8)+(-7.7))/(0.5)+(y*2.3*(-5.3)-y+(-y))*8.1)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-6.1)+(-q)-p)+(1.8*q+(-p)-(-4.8)+(-7.7))/(0.5)+(q*2.3*(-5.3)-q+(-q))*8.1)
            z2 <== (((-6.1)+(-y)-x)+(1.8*y+(-x)-(-4.8)+(-7.7))/(0.5)+(y*2.3*(-5.3)-y+(-y))*8.1)
            wr [I 575; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 576
        !"test576"
        //let z0 = (((-y)-(6.5-(-y)-4.6*(-x))-(y)+2.5-6.2*(-3.5))/0.5-((-6.1)+(6.1*0.5-y)+((-6.7)-(-x)/8.0-(-y)/(-0.3))/4.8))
        //printfn "%d" <| 576
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)-(6.5-(-y)-4.6*(-x))-(y)+2.5-6.2*(-3.5))/0.5-((-6.1)+(6.1*0.5-y)+((-6.7)-(-x)/8.0-(-y)/(-0.3))/4.8))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)-(6.5-(-q)-4.6*(-p))-(q)+2.5-6.2*(-3.5))/0.5-((-6.1)+(6.1*0.5-q)+((-6.7)-(-p)/8.0-(-q)/(-0.3))/4.8))
            z2 <== (((-y)-(6.5-(-y)-4.6*(-x))-(y)+2.5-6.2*(-3.5))/0.5-((-6.1)+(6.1*0.5-y)+((-6.7)-(-x)/8.0-(-y)/(-0.3))/4.8))
            wr [I 576; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 577
        !"test577"
        //let z0 = y
        //printfn "%d" <| 577
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 577; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 578
        !"test578"
        //let z0 = ((2.2+(-6.7)/4.6*((-1.1)-y)/6.4*x/((-y)+(-y)/(-y)+4.3))-(-4.3))
        //printfn "%d" <| 578
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((2.2+(-6.7)/4.6*((-1.1)-y)/6.4*x/((-y)+(-y)/(-y)+4.3))-(-4.3))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((2.2+(-6.7)/4.6*((-1.1)-q)/6.4*p/((-q)+(-q)/(-q)+4.3))-(-4.3))
            z2 <== ((2.2+(-6.7)/4.6*((-1.1)-y)/6.4*x/((-y)+(-y)/(-y)+4.3))-(-4.3))
            wr [I 578; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 579
        !"test579"
        //let z0 = (-7.0)
        //printfn "%d" <| 579
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 580
        !"test580"
        //let z0 = (((y)/y/y/(0.1))/(-3.0))
        //printfn "%d" <| 580
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((y)/y/y/(0.1))/(-3.0))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((q)/q/q/(0.1))/(-3.0))
            z2 <== (((y)/y/y/(0.1))/(-3.0))
            wr [I 580; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 581
        !"test581"
        //let z0 = (-8.1)
        //printfn "%d" <| 581
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 582
        !"test582"
        //let z0 = ((y*7.1+7.7/6.8*2.4)/(3.1*y/(-8.1)-(-y))+(-8.1))
        //printfn "%d" <| 582
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y*7.1+7.7/6.8*2.4)/(3.1*y/(-8.1)-(-y))+(-8.1))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q*7.1+7.7/6.8*2.4)/(3.1*q/(-8.1)-(-q))+(-8.1))
            z2 <== ((y*7.1+7.7/6.8*2.4)/(3.1*y/(-8.1)-(-y))+(-8.1))
            wr [I 582; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 583
        !"test583"
        //let z0 = ((-x)+x*2.4-(-x)/5.1/(((-3.4)-x/y)+y*(7.7+y*x-x)*(-8.4)))
        //printfn "%d" <| 583
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+x*2.4-(-x)/5.1/(((-3.4)-x/y)+y*(7.7+y*x-x)*(-8.4)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+p*2.4-(-p)/5.1/(((-3.4)-p/q)+q*(7.7+q*p-p)*(-8.4)))
            z2 <== ((-x)+x*2.4-(-x)/5.1/(((-3.4)-x/y)+y*(7.7+y*x-x)*(-8.4)))
            wr [I 583; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 584
        !"test584"
        //let z0 = (((-1.0)/4.7+((-x)*(-y)-y*x+x))+x+(-3.0))
        //printfn "%d" <| 584
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-1.0)/4.7+((-x)*(-y)-y*x+x))+x+(-3.0))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-1.0)/4.7+((-p)*(-q)-q*p+p))+p+(-3.0))
            z2 <== (((-1.0)/4.7+((-x)*(-y)-y*x+x))+x+(-3.0))
            wr [I 584; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 585
        !"test585"
        //let z0 = (-5.0)
        //printfn "%d" <| 585
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 586
        !"test586"
        //let z0 = (8.5/((-y))+(-5.2)*(((-8.8)-(-5.7)+2.7)+1.1+6.2-((-x)+(-x))*(-4.6))*x)
        //printfn "%d" <| 586
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((8.5/((-y))+(-5.2)*(((-8.8)-(-5.7)+2.7)+1.1+6.2-((-x)+(-x))*(-4.6))*x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (8.5/((-q))+(-5.2)*(((-8.8)-(-5.7)+2.7)+1.1+6.2-((-p)+(-p))*(-4.6))*p)
            z2 <== (8.5/((-y))+(-5.2)*(((-8.8)-(-5.7)+2.7)+1.1+6.2-((-x)+(-x))*(-4.6))*x)
            wr [I 586; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 587
        !"test587"
        //let z0 = x
        //printfn "%d" <| 587
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 587; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 588
        !"test588"
        //let z0 = ((-y)+((-2.1)*(-y)+(-4.5)))
        //printfn "%d" <| 588
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)+((-2.1)*(-y)+(-4.5)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)+((-2.1)*(-q)+(-4.5)))
            z2 <== ((-y)+((-2.1)*(-y)+(-4.5)))
            wr [I 588; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 589
        !"test589"
        //let z0 = ((3.6+y+((-x))*1.4*(-1.8))/((-y)/(-x)/(-y)/x-x+(-1.8)-y-(-4.1)+(-x))*((-4.6))/(((-x))-5.7*((-8.1)/6.4-x*(-x)-(-7.7))/((-y))*(x+(-y))))
        //printfn "%d" <| 589
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((3.6+y+((-x))*1.4*(-1.8))/((-y)/(-x)/(-y)/x-x+(-1.8)-y-(-4.1)+(-x))*((-4.6))/(((-x))-5.7*((-8.1)/6.4-x*(-x)-(-7.7))/((-y))*(x+(-y))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((3.6+q+((-p))*1.4*(-1.8))/((-q)/(-p)/(-q)/p-p+(-1.8)-q-(-4.1)+(-p))*((-4.6))/(((-p))-5.7*((-8.1)/6.4-p*(-p)-(-7.7))/((-q))*(p+(-q))))
            z2 <== ((3.6+y+((-x))*1.4*(-1.8))/((-y)/(-x)/(-y)/x-x+(-1.8)-y-(-4.1)+(-x))*((-4.6))/(((-x))-5.7*((-8.1)/6.4-x*(-x)-(-7.7))/((-y))*(x+(-y))))
            wr [I 589; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 590
        !"test590"
        //let z0 = (-6.7)
        //printfn "%d" <| 590
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 591
        !"test591"
        //let z0 = (-y)
        //printfn "%d" <| 591
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 591; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 592
        !"test592"
        //let z0 = y
        //printfn "%d" <| 592
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 592; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 593
        !"test593"
        //let z0 = (-x)
        //printfn "%d" <| 593
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 593; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 594
        !"test594"
        //let z0 = 7.3
        //printfn "%d" <| 594
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 595
        !"test595"
        //let z0 = ((3.3)*8.1+((-1.3)+((-4.3)+(-y)*y))+(-0.1)/y)
        //printfn "%d" <| 595
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((3.3)*8.1+((-1.3)+((-4.3)+(-y)*y))+(-0.1)/y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((3.3)*8.1+((-1.3)+((-4.3)+(-q)*q))+(-0.1)/q)
            z2 <== ((3.3)*8.1+((-1.3)+((-4.3)+(-y)*y))+(-0.1)/y)
            wr [I 595; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 596
        !"test596"
        //let z0 = (((-4.8)*y)/((-y)*(7.6))+(-6.6)*(y/y-(-y)/(-y))*(-x)*(-x)*1.0)
        //printfn "%d" <| 596
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-4.8)*y)/((-y)*(7.6))+(-6.6)*(y/y-(-y)/(-y))*(-x)*(-x)*1.0)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-4.8)*q)/((-q)*(7.6))+(-6.6)*(q/q-(-q)/(-q))*(-p)*(-p)*1.0)
            z2 <== (((-4.8)*y)/((-y)*(7.6))+(-6.6)*(y/y-(-y)/(-y))*(-x)*(-x)*1.0)
            wr [I 596; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 597
        !"test597"
        //let z0 = 3.7
        //printfn "%d" <| 597
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 598
        !"test598"
        //let z0 = 5.4
        //printfn "%d" <| 598
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 599
        !"test599"
        //let z0 = (4.7/(-6.0)*y*(-y))
        //printfn "%d" <| 599
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((4.7/(-6.0)*y*(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (4.7/(-6.0)*q*(-q))
            z2 <== (4.7/(-6.0)*y*(-y))
            wr [I 599; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 600
        !"test600"
        //let z0 = 5.1
        //printfn "%d" <| 600
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 601
        !"test601"
        //let z0 = ((x)-(-1.2)/(-8.8)-((-5.4)+x)-(-x)*(-0.3))
        //printfn "%d" <| 601
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x)-(-1.2)/(-8.8)-((-5.4)+x)-(-x)*(-0.3))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p)-(-1.2)/(-8.8)-((-5.4)+p)-(-p)*(-0.3))
            z2 <== ((x)-(-1.2)/(-8.8)-((-5.4)+x)-(-x)*(-0.3))
            wr [I 601; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 602
        !"test602"
        //let z0 = 7.4
        //printfn "%d" <| 602
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 603
        !"test603"
        //let z0 = ((((-4.0)+(-y))+3.7+(-y))+(((-8.2)+(-x))*y-6.4*(y/x/8.7/(-x))))
        //printfn "%d" <| 603
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-4.0)+(-y))+3.7+(-y))+(((-8.2)+(-x))*y-6.4*(y/x/8.7/(-x))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-4.0)+(-q))+3.7+(-q))+(((-8.2)+(-p))*q-6.4*(q/p/8.7/(-p))))
            z2 <== ((((-4.0)+(-y))+3.7+(-y))+(((-8.2)+(-x))*y-6.4*(y/x/8.7/(-x))))
            wr [I 603; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 604
        !"test604"
        //let z0 = (-2.4)
        //printfn "%d" <| 604
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 605
        !"test605"
        //let z0 = (-1.3)
        //printfn "%d" <| 605
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 606
        !"test606"
        //let z0 = (((-0.2)-(-x)*(-4.7)))
        //printfn "%d" <| 606
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-0.2)-(-x)*(-4.7)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-0.2)-(-p)*(-4.7)))
            z2 <== (((-0.2)-(-x)*(-4.7)))
            wr [I 606; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 607
        !"test607"
        //let z0 = ((-4.4)-(-7.7)+(-3.4))
        //printfn "%d" <| 607
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 608
        !"test608"
        //let z0 = (x+y-(-4.2)*(-4.8)*2.7-4.0/(-x)*6.5*(-x)-(-x)-x/(-x)/(8.5*y-4.2/y*3.8))-(y)-3.6+(-y)-7.5-x-(-y)
        //printfn "%d" <| 608
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x+y-(-4.2)*(-4.8)*2.7-4.0/(-x)*6.5*(-x)-(-x)-x/(-x)/(8.5*y-4.2/y*3.8))-(y)-3.6+(-y)-7.5-x-(-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p+q-(-4.2)*(-4.8)*2.7-4.0/(-p)*6.5*(-p)-(-p)-p/(-p)/(8.5*q-4.2/q*3.8))-(q)-3.6+(-q)-7.5-p-(-q)
            z2 <== (x+y-(-4.2)*(-4.8)*2.7-4.0/(-x)*6.5*(-x)-(-x)-x/(-x)/(8.5*y-4.2/y*3.8))-(y)-3.6+(-y)-7.5-x-(-y)
            wr [I 608; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 609
        !"test609"
        //let z0 = (((0.2))-4.7/1.3-y)
        //printfn "%d" <| 609
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((0.2))-4.7/1.3-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((0.2))-4.7/1.3-q)
            z2 <== (((0.2))-4.7/1.3-y)
            wr [I 609; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 610
        !"test610"
        //let z0 = (-y)
        //printfn "%d" <| 610
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 610; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 611
        !"test611"
        //let z0 = x
        //printfn "%d" <| 611
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 611; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 612
        !"test612"
        //let z0 = (-y)
        //printfn "%d" <| 612
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 612; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 613
        !"test613"
        //let z0 = (((x-(-y)+(-x)/(-6.3))+((-2.7))))
        //printfn "%d" <| 613
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((x-(-y)+(-x)/(-6.3))+((-2.7))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((p-(-q)+(-p)/(-6.3))+((-2.7))))
            z2 <== (((x-(-y)+(-x)/(-6.3))+((-2.7))))
            wr [I 613; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 614
        !"test614"
        //let z0 = ((-3.4)/((-y)/4.5-((-x))))
        //printfn "%d" <| 614
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-3.4)/((-y)/4.5-((-x))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-3.4)/((-q)/4.5-((-p))))
            z2 <== ((-3.4)/((-y)/4.5-((-x))))
            wr [I 614; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 615
        !"test615"
        //let z0 = ((((-y)/(-1.6)*(-0.8))*(-2.7))+(((-x)+(-8.7)/(-x)-(-y)))-(-x))
        //printfn "%d" <| 615
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-y)/(-1.6)*(-0.8))*(-2.7))+(((-x)+(-8.7)/(-x)-(-y)))-(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-q)/(-1.6)*(-0.8))*(-2.7))+(((-p)+(-8.7)/(-p)-(-q)))-(-p))
            z2 <== ((((-y)/(-1.6)*(-0.8))*(-2.7))+(((-x)+(-8.7)/(-x)-(-y)))-(-x))
            wr [I 615; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 616
        !"test616"
        //let z0 = x
        //printfn "%d" <| 616
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 616; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 617
        !"test617"
        //let z0 = 5.8
        //printfn "%d" <| 617
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 618
        !"test618"
        //let z0 = y
        //printfn "%d" <| 618
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 618; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 619
        !"test619"
        //let z0 = (-x)
        //printfn "%d" <| 619
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 619; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 620
        !"test620"
        //let z0 = ((-1.0)/(-x))
        //printfn "%d" <| 620
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-1.0)/(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-1.0)/(-p))
            z2 <== ((-1.0)/(-x))
            wr [I 620; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 621
        !"test621"
        //let z0 = ((-4.8))
        //printfn "%d" <| 621
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 622
        !"test622"
        //let z0 = ((-y)+y/y+(-7.7)+1.2/((-x))*((-5.8)-(7.0*7.6-(-x))/(y-(-x))-y)+(((-x)-3.0+0.2)/(-y)+(-0.6)*((-0.8)/(-5.7)-(-x)+x+(-x))-(-0.2)))
        //printfn "%d" <| 622
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)+y/y+(-7.7)+1.2/((-x))*((-5.8)-(7.0*7.6-(-x))/(y-(-x))-y)+(((-x)-3.0+0.2)/(-y)+(-0.6)*((-0.8)/(-5.7)-(-x)+x+(-x))-(-0.2)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)+q/q+(-7.7)+1.2/((-p))*((-5.8)-(7.0*7.6-(-p))/(q-(-p))-q)+(((-p)-3.0+0.2)/(-q)+(-0.6)*((-0.8)/(-5.7)-(-p)+p+(-p))-(-0.2)))
            z2 <== ((-y)+y/y+(-7.7)+1.2/((-x))*((-5.8)-(7.0*7.6-(-x))/(y-(-x))-y)+(((-x)-3.0+0.2)/(-y)+(-0.6)*((-0.8)/(-5.7)-(-x)+x+(-x))-(-0.2)))
            wr [I 622; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 623
        !"test623"
        //let z0 = (-7.0)
        //printfn "%d" <| 623
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 624
        !"test624"
        //let z0 = (((-1.4)*((-5.6)+y-(-6.4)*x*(-4.8))))
        //printfn "%d" <| 624
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-1.4)*((-5.6)+y-(-6.4)*x*(-4.8))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-1.4)*((-5.6)+q-(-6.4)*p*(-4.8))))
            z2 <== (((-1.4)*((-5.6)+y-(-6.4)*x*(-4.8))))
            wr [I 624; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 625
        !"test625"
        //let z0 = (-y)
        //printfn "%d" <| 625
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 625; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 626
        !"test626"
        //let z0 = (x-((y)))
        //printfn "%d" <| 626
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x-((y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p-((q)))
            z2 <== (x-((y)))
            wr [I 626; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 627
        !"test627"
        //let z0 = (-5.6)
        //printfn "%d" <| 627
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 628
        !"test628"
        //let z0 = 8.5
        //printfn "%d" <| 628
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 629
        !"test629"
        //let z0 = (-6.2)
        //printfn "%d" <| 629
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 630
        !"test630"
        //let z0 = (-y)
        //printfn "%d" <| 630
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 630; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 631
        !"test631"
        //let z0 = (-0.4)+7.7+(-x)
        //printfn "%d" <| 631
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-0.4)+7.7+(-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-0.4)+7.7+(-p)
            z2 <== (-0.4)+7.7+(-x)
            wr [I 631; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 632
        !"test632"
        //let z0 = (-2.0)
        //printfn "%d" <| 632
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 633
        !"test633"
        //let z0 = (((-0.1)/(-y)+(-y)*6.5*y/(x/x*(-8.8)))-(-2.3))
        //printfn "%d" <| 633
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-0.1)/(-y)+(-y)*6.5*y/(x/x*(-8.8)))-(-2.3))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-0.1)/(-q)+(-q)*6.5*q/(p/p*(-8.8)))-(-2.3))
            z2 <== (((-0.1)/(-y)+(-y)*6.5*y/(x/x*(-8.8)))-(-2.3))
            wr [I 633; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 634
        !"test634"
        //let z0 = x
        //printfn "%d" <| 634
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 634; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 635
        !"test635"
        //let z0 = (-0.6)
        //printfn "%d" <| 635
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 636
        !"test636"
        //let z0 = ((x/(-2.6))-(4.0*7.8)-(y)+((-3.0)*(-2.7)/y-y*x)-x*8.6/(-3.0))
        //printfn "%d" <| 636
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x/(-2.6))-(4.0*7.8)-(y)+((-3.0)*(-2.7)/y-y*x)-x*8.6/(-3.0))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p/(-2.6))-(4.0*7.8)-(q)+((-3.0)*(-2.7)/q-q*p)-p*8.6/(-3.0))
            z2 <== ((x/(-2.6))-(4.0*7.8)-(y)+((-3.0)*(-2.7)/y-y*x)-x*8.6/(-3.0))
            wr [I 636; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 637
        !"test637"
        //let z0 = x
        //printfn "%d" <| 637
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 637; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 638
        !"test638"
        //let z0 = 3.4
        //printfn "%d" <| 638
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 639
        !"test639"
        //let z0 = (-7.6)
        //printfn "%d" <| 639
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 640
        !"test640"
        //let z0 = (-y)
        //printfn "%d" <| 640
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 640; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 641
        !"test641"
        //let z0 = (y-(-8.5))
        //printfn "%d" <| 641
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y-(-8.5))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q-(-8.5))
            z2 <== (y-(-8.5))
            wr [I 641; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 642
        !"test642"
        //let z0 = (3.6)
        //printfn "%d" <| 642
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 643
        !"test643"
        //let z0 = (((-0.3)/((-x))*4.5)*(3.7*y+5.3+7.5+x)+((-4.5)+(-x)*(-7.7))*x-(-y)+(y/((-x)+1.3-(-y)*(-y)+(-x))/((-2.6)*x*y/(-y))/((-x)))*(-y))
        //printfn "%d" <| 643
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-0.3)/((-x))*4.5)*(3.7*y+5.3+7.5+x)+((-4.5)+(-x)*(-7.7))*x-(-y)+(y/((-x)+1.3-(-y)*(-y)+(-x))/((-2.6)*x*y/(-y))/((-x)))*(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-0.3)/((-p))*4.5)*(3.7*q+5.3+7.5+p)+((-4.5)+(-p)*(-7.7))*p-(-q)+(q/((-p)+1.3-(-q)*(-q)+(-p))/((-2.6)*p*q/(-q))/((-p)))*(-q))
            z2 <== (((-0.3)/((-x))*4.5)*(3.7*y+5.3+7.5+x)+((-4.5)+(-x)*(-7.7))*x-(-y)+(y/((-x)+1.3-(-y)*(-y)+(-x))/((-2.6)*x*y/(-y))/((-x)))*(-y))
            wr [I 643; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 644
        !"test644"
        //let z0 = (-x)
        //printfn "%d" <| 644
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 644; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 645
        !"test645"
        //let z0 = (5.5/(-x)-(-7.6)+x-(y/(y-7.2))-(-y))
        //printfn "%d" <| 645
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.5/(-x)-(-7.6)+x-(y/(y-7.2))-(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.5/(-p)-(-7.6)+p-(q/(q-7.2))-(-q))
            z2 <== (5.5/(-x)-(-7.6)+x-(y/(y-7.2))-(-y))
            wr [I 645; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 646
        !"test646"
        //let z0 = (-x)
        //printfn "%d" <| 646
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 646; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 647
        !"test647"
        //let z0 = (-4.6)/(-x)-(((-x)+6.8)+(y-(-x)+x+2.3/y)*(-8.5)*x*((-y)*3.0+(-y)-7.7))
        //printfn "%d" <| 647
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-4.6)/(-x)-(((-x)+6.8)+(y-(-x)+x+2.3/y)*(-8.5)*x*((-y)*3.0+(-y)-7.7))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-4.6)/(-p)-(((-p)+6.8)+(q-(-p)+p+2.3/q)*(-8.5)*p*((-q)*3.0+(-q)-7.7))
            z2 <== (-4.6)/(-x)-(((-x)+6.8)+(y-(-x)+x+2.3/y)*(-8.5)*x*((-y)*3.0+(-y)-7.7))
            wr [I 647; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 648
        !"test648"
        //let z0 = ((-y)+(-2.7)+((y+(-x))*(2.3/3.8/1.1)+y-(-7.3)+x-7.4/(-y))+1.6-0.3)
        //printfn "%d" <| 648
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)+(-2.7)+((y+(-x))*(2.3/3.8/1.1)+y-(-7.3)+x-7.4/(-y))+1.6-0.3)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)+(-2.7)+((q+(-p))*(2.3/3.8/1.1)+q-(-7.3)+p-7.4/(-q))+1.6-0.3)
            z2 <== ((-y)+(-2.7)+((y+(-x))*(2.3/3.8/1.1)+y-(-7.3)+x-7.4/(-y))+1.6-0.3)
            wr [I 648; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 649
        !"test649"
        //let z0 = (-7.4)
        //printfn "%d" <| 649
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 650
        !"test650"
        //let z0 = (4.1*(8.8*((-x)/6.7+(-x)-x))*(3.0/(6.5-(-y)+x)+(-y)-2.4-x)-(2.2))
        //printfn "%d" <| 650
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((4.1*(8.8*((-x)/6.7+(-x)-x))*(3.0/(6.5-(-y)+x)+(-y)-2.4-x)-(2.2))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (4.1*(8.8*((-p)/6.7+(-p)-p))*(3.0/(6.5-(-q)+p)+(-q)-2.4-p)-(2.2))
            z2 <== (4.1*(8.8*((-x)/6.7+(-x)-x))*(3.0/(6.5-(-y)+x)+(-y)-2.4-x)-(2.2))
            wr [I 650; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 651
        !"test651"
        //let z0 = (-7.5)
        //printfn "%d" <| 651
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 652
        !"test652"
        //let z0 = ((y/y-(-y)*(5.1-(-6.6)*(-6.6)+x-y))/4.0*((x/x+(-x)*(-6.5)*y))-((-3.7)*((-y)+(-1.6)-(-y))/(-3.8)-((-5.4)/3.3/x/(-3.1))+(-x)))
        //printfn "%d" <| 652
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y/y-(-y)*(5.1-(-6.6)*(-6.6)+x-y))/4.0*((x/x+(-x)*(-6.5)*y))-((-3.7)*((-y)+(-1.6)-(-y))/(-3.8)-((-5.4)/3.3/x/(-3.1))+(-x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q/q-(-q)*(5.1-(-6.6)*(-6.6)+p-q))/4.0*((p/p+(-p)*(-6.5)*q))-((-3.7)*((-q)+(-1.6)-(-q))/(-3.8)-((-5.4)/3.3/p/(-3.1))+(-p)))
            z2 <== ((y/y-(-y)*(5.1-(-6.6)*(-6.6)+x-y))/4.0*((x/x+(-x)*(-6.5)*y))-((-3.7)*((-y)+(-1.6)-(-y))/(-3.8)-((-5.4)/3.3/x/(-3.1))+(-x)))
            wr [I 652; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 653
        !"test653"
        //let z0 = (-y)/(-6.5)-((x-(-4.4)+(-x)+y/y)*(-0.7)/(-7.6)-(x)*(-x))-5.4/3.1
        //printfn "%d" <| 653
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)/(-6.5)-((x-(-4.4)+(-x)+y/y)*(-0.7)/(-7.6)-(x)*(-x))-5.4/3.1).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)/(-6.5)-((p-(-4.4)+(-p)+q/q)*(-0.7)/(-7.6)-(p)*(-p))-5.4/3.1
            z2 <== (-y)/(-6.5)-((x-(-4.4)+(-x)+y/y)*(-0.7)/(-7.6)-(x)*(-x))-5.4/3.1
            wr [I 653; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 654
        !"test654"
        //let z0 = 1.7
        //printfn "%d" <| 654
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 655
        !"test655"
        //let z0 = (((-y)+(-x))-(x-7.5+x*(-y)*y)/(-2.0)/((-y)-4.0*(-3.0)+6.1/(-x))+(((-6.6)+x-2.3*y)*(6.2)+(-7.2))/y)
        //printfn "%d" <| 655
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)+(-x))-(x-7.5+x*(-y)*y)/(-2.0)/((-y)-4.0*(-3.0)+6.1/(-x))+(((-6.6)+x-2.3*y)*(6.2)+(-7.2))/y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)+(-p))-(p-7.5+p*(-q)*q)/(-2.0)/((-q)-4.0*(-3.0)+6.1/(-p))+(((-6.6)+p-2.3*q)*(6.2)+(-7.2))/q)
            z2 <== (((-y)+(-x))-(x-7.5+x*(-y)*y)/(-2.0)/((-y)-4.0*(-3.0)+6.1/(-x))+(((-6.6)+x-2.3*y)*(6.2)+(-7.2))/y)
            wr [I 655; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 656
        !"test656"
        //let z0 = (-0.3)
        //printfn "%d" <| 656
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 657
        !"test657"
        //let z0 = x
        //printfn "%d" <| 657
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 657; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 658
        !"test658"
        //let z0 = ((((-4.3)/x*5.3/(-x)-0.1)/7.2)*6.6)
        //printfn "%d" <| 658
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-4.3)/x*5.3/(-x)-0.1)/7.2)*6.6)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-4.3)/p*5.3/(-p)-0.1)/7.2)*6.6)
            z2 <== ((((-4.3)/x*5.3/(-x)-0.1)/7.2)*6.6)
            wr [I 658; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 659
        !"test659"
        //let z0 = (-x)
        //printfn "%d" <| 659
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 659; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 660
        !"test660"
        //let z0 = ((((-x)/1.6+4.4*(-1.0)-x)/((-x)+x-(-1.6)+(-x)-8.0))*y)
        //printfn "%d" <| 660
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-x)/1.6+4.4*(-1.0)-x)/((-x)+x-(-1.6)+(-x)-8.0))*y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-p)/1.6+4.4*(-1.0)-p)/((-p)+p-(-1.6)+(-p)-8.0))*q)
            z2 <== ((((-x)/1.6+4.4*(-1.0)-x)/((-x)+x-(-1.6)+(-x)-8.0))*y)
            wr [I 660; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 661
        !"test661"
        //let z0 = 6.5
        //printfn "%d" <| 661
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 662
        !"test662"
        //let z0 = (((4.3-(-6.7))-((-x)-(-y)*y)+(-x)-(y)/(3.8/5.6))/x)
        //printfn "%d" <| 662
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((4.3-(-6.7))-((-x)-(-y)*y)+(-x)-(y)/(3.8/5.6))/x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((4.3-(-6.7))-((-p)-(-q)*q)+(-p)-(q)/(3.8/5.6))/p)
            z2 <== (((4.3-(-6.7))-((-x)-(-y)*y)+(-x)-(y)/(3.8/5.6))/x)
            wr [I 662; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 663
        !"test663"
        //let z0 = (5.0/(-6.6)+((-y)-((-3.1)/(-1.7)-(-0.5)+5.0)*6.5*((-x)))-(((-x)-(-y)/(-6.8)+x)/(0.1-(-4.0)+(-x)-8.6)/(x-(-6.1)+(-y)+5.6)))
        //printfn "%d" <| 663
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.0/(-6.6)+((-y)-((-3.1)/(-1.7)-(-0.5)+5.0)*6.5*((-x)))-(((-x)-(-y)/(-6.8)+x)/(0.1-(-4.0)+(-x)-8.6)/(x-(-6.1)+(-y)+5.6)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.0/(-6.6)+((-q)-((-3.1)/(-1.7)-(-0.5)+5.0)*6.5*((-p)))-(((-p)-(-q)/(-6.8)+p)/(0.1-(-4.0)+(-p)-8.6)/(p-(-6.1)+(-q)+5.6)))
            z2 <== (5.0/(-6.6)+((-y)-((-3.1)/(-1.7)-(-0.5)+5.0)*6.5*((-x)))-(((-x)-(-y)/(-6.8)+x)/(0.1-(-4.0)+(-x)-8.6)/(x-(-6.1)+(-y)+5.6)))
            wr [I 663; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 664
        !"test664"
        //let z0 = (-y)
        //printfn "%d" <| 664
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 664; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 665
        !"test665"
        //let z0 = (-2.1)
        //printfn "%d" <| 665
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 666
        !"test666"
        //let z0 = (-y)
        //printfn "%d" <| 666
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 666; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 667
        !"test667"
        //let z0 = 8.6
        //printfn "%d" <| 667
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 668
        !"test668"
        //let z0 = ((4.5-(-y)-(-5.2)/(-y)/(x))+x/y)
        //printfn "%d" <| 668
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((4.5-(-y)-(-5.2)/(-y)/(x))+x/y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((4.5-(-q)-(-5.2)/(-q)/(p))+p/q)
            z2 <== ((4.5-(-y)-(-5.2)/(-y)/(x))+x/y)
            wr [I 668; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 669
        !"test669"
        //let z0 = (-6.4)
        //printfn "%d" <| 669
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 670
        !"test670"
        //let z0 = ((y+(x)-(y-x)+(-5.3)+y)+(((-y)+(-5.8)+(-y)/1.7/y)/((-x)+(-y)+(-2.6)*1.1)))
        //printfn "%d" <| 670
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y+(x)-(y-x)+(-5.3)+y)+(((-y)+(-5.8)+(-y)/1.7/y)/((-x)+(-y)+(-2.6)*1.1)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q+(p)-(q-p)+(-5.3)+q)+(((-q)+(-5.8)+(-q)/1.7/q)/((-p)+(-q)+(-2.6)*1.1)))
            z2 <== ((y+(x)-(y-x)+(-5.3)+y)+(((-y)+(-5.8)+(-y)/1.7/y)/((-x)+(-y)+(-2.6)*1.1)))
            wr [I 670; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 671
        !"test671"
        //let z0 = (-2.3)
        //printfn "%d" <| 671
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 672
        !"test672"
        //let z0 = ((-x)-(x)+((-7.5)*(-x)-x-x/(-8.7))+(x+(-3.8))/6.3)
        //printfn "%d" <| 672
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)-(x)+((-7.5)*(-x)-x-x/(-8.7))+(x+(-3.8))/6.3)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)-(p)+((-7.5)*(-p)-p-p/(-8.7))+(p+(-3.8))/6.3)
            z2 <== ((-x)-(x)+((-7.5)*(-x)-x-x/(-8.7))+(x+(-3.8))/6.3)
            wr [I 672; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 673
        !"test673"
        //let z0 = (2.8+(-8.2)-((-3.0)/y*x+x)-y*(-x))
        //printfn "%d" <| 673
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((2.8+(-8.2)-((-3.0)/y*x+x)-y*(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (2.8+(-8.2)-((-3.0)/q*p+p)-q*(-p))
            z2 <== (2.8+(-8.2)-((-3.0)/y*x+x)-y*(-x))
            wr [I 673; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 674
        !"test674"
        //let z0 = (0.2+x*y+(y*(-x)*(-6.6))*((-x)/(-y)+x/2.7/y)-5.3*(((-y)/y*y/7.4+8.8)+(-x)))
        //printfn "%d" <| 674
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((0.2+x*y+(y*(-x)*(-6.6))*((-x)/(-y)+x/2.7/y)-5.3*(((-y)/y*y/7.4+8.8)+(-x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (0.2+p*q+(q*(-p)*(-6.6))*((-p)/(-q)+p/2.7/q)-5.3*(((-q)/q*q/7.4+8.8)+(-p)))
            z2 <== (0.2+x*y+(y*(-x)*(-6.6))*((-x)/(-y)+x/2.7/y)-5.3*(((-y)/y*y/7.4+8.8)+(-x)))
            wr [I 674; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 675
        !"test675"
        //let z0 = (y/((5.6-4.2*(-y)-(-x)*x)+(-y)-(-y)+((-x)*(-y)/5.4+1.5*(-x))-(7.4*x))-(x*((-y)-(-y)-2.5-(-7.1)*3.5)*8.3))
        //printfn "%d" <| 675
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y/((5.6-4.2*(-y)-(-x)*x)+(-y)-(-y)+((-x)*(-y)/5.4+1.5*(-x))-(7.4*x))-(x*((-y)-(-y)-2.5-(-7.1)*3.5)*8.3))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q/((5.6-4.2*(-q)-(-p)*p)+(-q)-(-q)+((-p)*(-q)/5.4+1.5*(-p))-(7.4*p))-(p*((-q)-(-q)-2.5-(-7.1)*3.5)*8.3))
            z2 <== (y/((5.6-4.2*(-y)-(-x)*x)+(-y)-(-y)+((-x)*(-y)/5.4+1.5*(-x))-(7.4*x))-(x*((-y)-(-y)-2.5-(-7.1)*3.5)*8.3))
            wr [I 675; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 676
        !"test676"
        //let z0 = (-y)
        //printfn "%d" <| 676
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 676; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 677
        !"test677"
        //let z0 = (((8.0+(-5.5))-(y-(-x)))+2.2*x-(-x)+(6.5-(-y)/x)+(-y)*((-7.4)+(-4.7)+(-2.5))+(8.2/y*(-x))-4.8)
        //printfn "%d" <| 677
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((8.0+(-5.5))-(y-(-x)))+2.2*x-(-x)+(6.5-(-y)/x)+(-y)*((-7.4)+(-4.7)+(-2.5))+(8.2/y*(-x))-4.8)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((8.0+(-5.5))-(q-(-p)))+2.2*p-(-p)+(6.5-(-q)/p)+(-q)*((-7.4)+(-4.7)+(-2.5))+(8.2/q*(-p))-4.8)
            z2 <== (((8.0+(-5.5))-(y-(-x)))+2.2*x-(-x)+(6.5-(-y)/x)+(-y)*((-7.4)+(-4.7)+(-2.5))+(8.2/y*(-x))-4.8)
            wr [I 677; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 678
        !"test678"
        //let z0 = ((-x)+(((-x))/((-4.4)*x-(-x)/(-x))*(-x))/((6.6/8.0)-(x/(-4.7)*(-x)-(-y))))
        //printfn "%d" <| 678
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+(((-x))/((-4.4)*x-(-x)/(-x))*(-x))/((6.6/8.0)-(x/(-4.7)*(-x)-(-y))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+(((-p))/((-4.4)*p-(-p)/(-p))*(-p))/((6.6/8.0)-(p/(-4.7)*(-p)-(-q))))
            z2 <== ((-x)+(((-x))/((-4.4)*x-(-x)/(-x))*(-x))/((6.6/8.0)-(x/(-4.7)*(-x)-(-y))))
            wr [I 678; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 679
        !"test679"
        //let z0 = 0.8
        //printfn "%d" <| 679
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 680
        !"test680"
        //let z0 = 1.6
        //printfn "%d" <| 680
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 681
        !"test681"
        //let z0 = y
        //printfn "%d" <| 681
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 681; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 682
        !"test682"
        //let z0 = (0.6+(-7.4)*1.5)
        //printfn "%d" <| 682
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 683
        !"test683"
        //let z0 = (((-4.1)+6.6*4.1+(-y))/(-x)/((-y)*x*(-5.0)/(-1.6)-(-x))-(-x)/((x/0.8)-((-3.8)-0.6-y+2.3*(-x))-1.6-(-8.3))/((4.5+0.5+(-x)-6.2)/((-x)-(-x)-6.1/8.5)*((-1.0)*y/(-y)+y-3.4)/((-x)+x+(-y))))
        //printfn "%d" <| 683
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-4.1)+6.6*4.1+(-y))/(-x)/((-y)*x*(-5.0)/(-1.6)-(-x))-(-x)/((x/0.8)-((-3.8)-0.6-y+2.3*(-x))-1.6-(-8.3))/((4.5+0.5+(-x)-6.2)/((-x)-(-x)-6.1/8.5)*((-1.0)*y/(-y)+y-3.4)/((-x)+x+(-y))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-4.1)+6.6*4.1+(-q))/(-p)/((-q)*p*(-5.0)/(-1.6)-(-p))-(-p)/((p/0.8)-((-3.8)-0.6-q+2.3*(-p))-1.6-(-8.3))/((4.5+0.5+(-p)-6.2)/((-p)-(-p)-6.1/8.5)*((-1.0)*q/(-q)+q-3.4)/((-p)+p+(-q))))
            z2 <== (((-4.1)+6.6*4.1+(-y))/(-x)/((-y)*x*(-5.0)/(-1.6)-(-x))-(-x)/((x/0.8)-((-3.8)-0.6-y+2.3*(-x))-1.6-(-8.3))/((4.5+0.5+(-x)-6.2)/((-x)-(-x)-6.1/8.5)*((-1.0)*y/(-y)+y-3.4)/((-x)+x+(-y))))
            wr [I 683; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 684
        !"test684"
        //let z0 = (-x)
        //printfn "%d" <| 684
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 684; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 685
        !"test685"
        //let z0 = (-3.2)/(-8.5)
        //printfn "%d" <| 685
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 686
        !"test686"
        //let z0 = 5.8
        //printfn "%d" <| 686
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 687
        !"test687"
        //let z0 = (x/(-1.3)-0.5*(-8.4)*y-(y/x*(-0.1))*0.0+((-x)*x-(-x)*2.2+y)/(-y))*7.8
        //printfn "%d" <| 687
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x/(-1.3)-0.5*(-8.4)*y-(y/x*(-0.1))*0.0+((-x)*x-(-x)*2.2+y)/(-y))*7.8).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p/(-1.3)-0.5*(-8.4)*q-(q/p*(-0.1))*0.0+((-p)*p-(-p)*2.2+q)/(-q))*7.8
            z2 <== (x/(-1.3)-0.5*(-8.4)*y-(y/x*(-0.1))*0.0+((-x)*x-(-x)*2.2+y)/(-y))*7.8
            wr [I 687; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 688
        !"test688"
        //let z0 = 1.1
        //printfn "%d" <| 688
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 689
        !"test689"
        //let z0 = ((-0.2))
        //printfn "%d" <| 689
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 690
        !"test690"
        //let z0 = (x+(((-1.1))+(-x))/(0.3))
        //printfn "%d" <| 690
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x+(((-1.1))+(-x))/(0.3))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p+(((-1.1))+(-p))/(0.3))
            z2 <== (x+(((-1.1))+(-x))/(0.3))
            wr [I 690; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 691
        !"test691"
        //let z0 = (-1.6)
        //printfn "%d" <| 691
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 692
        !"test692"
        //let z0 = ((7.1-x+(8.5)+(x-8.4*(-y)))+(-7.1)+(-y)+(-y)+(y+x+((-7.8)+8.4+x)/((-6.7))/y+2.5/(-2.6)*(7.5*(-y)-x*1.5-(-x))))
        //printfn "%d" <| 692
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((7.1-x+(8.5)+(x-8.4*(-y)))+(-7.1)+(-y)+(-y)+(y+x+((-7.8)+8.4+x)/((-6.7))/y+2.5/(-2.6)*(7.5*(-y)-x*1.5-(-x))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((7.1-p+(8.5)+(p-8.4*(-q)))+(-7.1)+(-q)+(-q)+(q+p+((-7.8)+8.4+p)/((-6.7))/q+2.5/(-2.6)*(7.5*(-q)-p*1.5-(-p))))
            z2 <== ((7.1-x+(8.5)+(x-8.4*(-y)))+(-7.1)+(-y)+(-y)+(y+x+((-7.8)+8.4+x)/((-6.7))/y+2.5/(-2.6)*(7.5*(-y)-x*1.5-(-x))))
            wr [I 692; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 693
        !"test693"
        //let z0 = ((-1.6)*(((-y)/x*(-x)/(-y))*(x+(-3.6)-5.1-0.4*x)*(-y)-x)+y)
        //printfn "%d" <| 693
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-1.6)*(((-y)/x*(-x)/(-y))*(x+(-3.6)-5.1-0.4*x)*(-y)-x)+y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-1.6)*(((-q)/p*(-p)/(-q))*(p+(-3.6)-5.1-0.4*p)*(-q)-p)+q)
            z2 <== ((-1.6)*(((-y)/x*(-x)/(-y))*(x+(-3.6)-5.1-0.4*x)*(-y)-x)+y)
            wr [I 693; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 694
        !"test694"
        //let z0 = (-7.1)
        //printfn "%d" <| 694
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 695
        !"test695"
        //let z0 = (y*(-y)-(y+0.6+8.0-(-y))/(4.2/x*(-2.7)/4.2+(-y))*(-5.7)/(-y))
        //printfn "%d" <| 695
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y*(-y)-(y+0.6+8.0-(-y))/(4.2/x*(-2.7)/4.2+(-y))*(-5.7)/(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q*(-q)-(q+0.6+8.0-(-q))/(4.2/p*(-2.7)/4.2+(-q))*(-5.7)/(-q))
            z2 <== (y*(-y)-(y+0.6+8.0-(-y))/(4.2/x*(-2.7)/4.2+(-y))*(-5.7)/(-y))
            wr [I 695; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 696
        !"test696"
        //let z0 = (-5.0)
        //printfn "%d" <| 696
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 697
        !"test697"
        //let z0 = ((-y)+3.4*3.7+(3.4/8.1+2.1)/(7.3)/(2.3-y*y*(-x))*(x))
        //printfn "%d" <| 697
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)+3.4*3.7+(3.4/8.1+2.1)/(7.3)/(2.3-y*y*(-x))*(x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)+3.4*3.7+(3.4/8.1+2.1)/(7.3)/(2.3-q*q*(-p))*(p))
            z2 <== ((-y)+3.4*3.7+(3.4/8.1+2.1)/(7.3)/(2.3-y*y*(-x))*(x))
            wr [I 697; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 698
        !"test698"
        //let z0 = y
        //printfn "%d" <| 698
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 698; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 699
        !"test699"
        //let z0 = 7.5
        //printfn "%d" <| 699
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 700
        !"test700"
        //let z0 = (-y)
        //printfn "%d" <| 700
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 700; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 701
        !"test701"
        //let z0 = (-y)
        //printfn "%d" <| 701
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 701; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 702
        !"test702"
        //let z0 = (((-x)-((-x)/x-(-y)+8.5+y)-(y)-(-8.0))/(-5.5)*y+x)
        //printfn "%d" <| 702
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)-((-x)/x-(-y)+8.5+y)-(y)-(-8.0))/(-5.5)*y+x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)-((-p)/p-(-q)+8.5+q)-(q)-(-8.0))/(-5.5)*q+p)
            z2 <== (((-x)-((-x)/x-(-y)+8.5+y)-(y)-(-8.0))/(-5.5)*y+x)
            wr [I 702; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 703
        !"test703"
        //let z0 = ((-1.2)*((-y)+(-y)-y+7.7*(-8.7))/(x*(-x)/y*(-x)/(-6.4))-((-x)/(-8.7)/2.5*(-y)-6.1)*2.3-(-0.4)/y*((-8.1))-x/(-y))
        //printfn "%d" <| 703
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-1.2)*((-y)+(-y)-y+7.7*(-8.7))/(x*(-x)/y*(-x)/(-6.4))-((-x)/(-8.7)/2.5*(-y)-6.1)*2.3-(-0.4)/y*((-8.1))-x/(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-1.2)*((-q)+(-q)-q+7.7*(-8.7))/(p*(-p)/q*(-p)/(-6.4))-((-p)/(-8.7)/2.5*(-q)-6.1)*2.3-(-0.4)/q*((-8.1))-p/(-q))
            z2 <== ((-1.2)*((-y)+(-y)-y+7.7*(-8.7))/(x*(-x)/y*(-x)/(-6.4))-((-x)/(-8.7)/2.5*(-y)-6.1)*2.3-(-0.4)/y*((-8.1))-x/(-y))
            wr [I 703; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 704
        !"test704"
        //let z0 = ((-4.3)/(-2.6)+((-x)+(5.7+(-5.1)-0.5-(-5.5)+y)*(-6.8)/(-x)))
        //printfn "%d" <| 704
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-4.3)/(-2.6)+((-x)+(5.7+(-5.1)-0.5-(-5.5)+y)*(-6.8)/(-x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-4.3)/(-2.6)+((-p)+(5.7+(-5.1)-0.5-(-5.5)+q)*(-6.8)/(-p)))
            z2 <== ((-4.3)/(-2.6)+((-x)+(5.7+(-5.1)-0.5-(-5.5)+y)*(-6.8)/(-x)))
            wr [I 704; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 705
        !"test705"
        //let z0 = (3.6)
        //printfn "%d" <| 705
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 706
        !"test706"
        //let z0 = x
        //printfn "%d" <| 706
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 706; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 707
        !"test707"
        //let z0 = (-y)
        //printfn "%d" <| 707
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 707; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 708
        !"test708"
        //let z0 = x
        //printfn "%d" <| 708
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 708; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 709
        !"test709"
        //let z0 = ((-x))
        //printfn "%d" <| 709
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p))
            z2 <== ((-x))
            wr [I 709; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 710
        !"test710"
        //let z0 = 3.5
        //printfn "%d" <| 710
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 711
        !"test711"
        //let z0 = ((-6.4)/x-(-5.6))-((-y)/x)-((-y)*5.8)*(2.1-(7.4)-7.8*(-x)/3.8)/(-x)-(y*(-x)-8.8-(-3.7)+(-x)+y-(-4.3)+3.4)
        //printfn "%d" <| 711
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-6.4)/x-(-5.6))-((-y)/x)-((-y)*5.8)*(2.1-(7.4)-7.8*(-x)/3.8)/(-x)-(y*(-x)-8.8-(-3.7)+(-x)+y-(-4.3)+3.4)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-6.4)/p-(-5.6))-((-q)/p)-((-q)*5.8)*(2.1-(7.4)-7.8*(-p)/3.8)/(-p)-(q*(-p)-8.8-(-3.7)+(-p)+q-(-4.3)+3.4)
            z2 <== ((-6.4)/x-(-5.6))-((-y)/x)-((-y)*5.8)*(2.1-(7.4)-7.8*(-x)/3.8)/(-x)-(y*(-x)-8.8-(-3.7)+(-x)+y-(-4.3)+3.4)
            wr [I 711; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 712
        !"test712"
        //let z0 = (y-(-x)/2.4/(-x)+5.1)
        //printfn "%d" <| 712
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y-(-x)/2.4/(-x)+5.1)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q-(-p)/2.4/(-p)+5.1)
            z2 <== (y-(-x)/2.4/(-x)+5.1)
            wr [I 712; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 713
        !"test713"
        //let z0 = (-5.8)
        //printfn "%d" <| 713
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 714
        !"test714"
        //let z0 = (-y)
        //printfn "%d" <| 714
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 714; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 715
        !"test715"
        //let z0 = ((x)+(-y))
        //printfn "%d" <| 715
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x)+(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p)+(-q))
            z2 <== ((x)+(-y))
            wr [I 715; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 716
        !"test716"
        //let z0 = (y/(-4.1)-(-4.3)*4.7+(((-x))))
        //printfn "%d" <| 716
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y/(-4.1)-(-4.3)*4.7+(((-x))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q/(-4.1)-(-4.3)*4.7+(((-p))))
            z2 <== (y/(-4.1)-(-4.3)*4.7+(((-x))))
            wr [I 716; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 717
        !"test717"
        //let z0 = (-1.2)
        //printfn "%d" <| 717
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 718
        !"test718"
        //let z0 = x
        //printfn "%d" <| 718
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 718; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 719
        !"test719"
        //let z0 = 8.8
        //printfn "%d" <| 719
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 720
        !"test720"
        //let z0 = (-8.3)
        //printfn "%d" <| 720
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 721
        !"test721"
        //let z0 = (((-4.0)-y+3.2-x*y)/((-2.8)+((-2.3)/(-8.6)+6.7-(-x))/(-7.6)/y)-(-x)/x+(-x)+(y-4.8-3.3-x*y)*x/((-2.0)/4.2-(-6.0)*(-1.5)*(-x))*6.3-(((-0.8)+(-x)+(-y)+(-1.3))))
        //printfn "%d" <| 721
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-4.0)-y+3.2-x*y)/((-2.8)+((-2.3)/(-8.6)+6.7-(-x))/(-7.6)/y)-(-x)/x+(-x)+(y-4.8-3.3-x*y)*x/((-2.0)/4.2-(-6.0)*(-1.5)*(-x))*6.3-(((-0.8)+(-x)+(-y)+(-1.3))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-4.0)-q+3.2-p*q)/((-2.8)+((-2.3)/(-8.6)+6.7-(-p))/(-7.6)/q)-(-p)/p+(-p)+(q-4.8-3.3-p*q)*p/((-2.0)/4.2-(-6.0)*(-1.5)*(-p))*6.3-(((-0.8)+(-p)+(-q)+(-1.3))))
            z2 <== (((-4.0)-y+3.2-x*y)/((-2.8)+((-2.3)/(-8.6)+6.7-(-x))/(-7.6)/y)-(-x)/x+(-x)+(y-4.8-3.3-x*y)*x/((-2.0)/4.2-(-6.0)*(-1.5)*(-x))*6.3-(((-0.8)+(-x)+(-y)+(-1.3))))
            wr [I 721; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 722
        !"test722"
        //let z0 = x*(7.2)+(-x)*(4.2*(-2.0)-(-0.2))/x
        //printfn "%d" <| 722
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x*(7.2)+(-x)*(4.2*(-2.0)-(-0.2))/x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p*(7.2)+(-p)*(4.2*(-2.0)-(-0.2))/p
            z2 <== x*(7.2)+(-x)*(4.2*(-2.0)-(-0.2))/x
            wr [I 722; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 723
        !"test723"
        //let z0 = ((-x)/(-4.2))/(-4.1)/x*(-8.5)*(x*4.0)+((-x)*(-3.5)/(-x))+y*(-0.7)
        //printfn "%d" <| 723
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)/(-4.2))/(-4.1)/x*(-8.5)*(x*4.0)+((-x)*(-3.5)/(-x))+y*(-0.7)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)/(-4.2))/(-4.1)/p*(-8.5)*(p*4.0)+((-p)*(-3.5)/(-p))+q*(-0.7)
            z2 <== ((-x)/(-4.2))/(-4.1)/x*(-8.5)*(x*4.0)+((-x)*(-3.5)/(-x))+y*(-0.7)
            wr [I 723; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 724
        !"test724"
        //let z0 = ((-0.6)*1.7*3.8-(y-(-x)+(-x)*y-(-x))/x+0.4)
        //printfn "%d" <| 724
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.6)*1.7*3.8-(y-(-x)+(-x)*y-(-x))/x+0.4)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.6)*1.7*3.8-(q-(-p)+(-p)*q-(-p))/p+0.4)
            z2 <== ((-0.6)*1.7*3.8-(y-(-x)+(-x)*y-(-x))/x+0.4)
            wr [I 724; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 725
        !"test725"
        //let z0 = ((-7.5)+8.4+((-y)/y*(-x)*(-y)))-(x-((-y)/(-8.6)/(-5.6))-(6.1/(-1.2)*(-2.8)*5.4)/((-2.1)+y-(-6.6)*6.1))
        //printfn "%d" <| 725
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.5)+8.4+((-y)/y*(-x)*(-y)))-(x-((-y)/(-8.6)/(-5.6))-(6.1/(-1.2)*(-2.8)*5.4)/((-2.1)+y-(-6.6)*6.1))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.5)+8.4+((-q)/q*(-p)*(-q)))-(p-((-q)/(-8.6)/(-5.6))-(6.1/(-1.2)*(-2.8)*5.4)/((-2.1)+q-(-6.6)*6.1))
            z2 <== ((-7.5)+8.4+((-y)/y*(-x)*(-y)))-(x-((-y)/(-8.6)/(-5.6))-(6.1/(-1.2)*(-2.8)*5.4)/((-2.1)+y-(-6.6)*6.1))
            wr [I 725; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 726
        !"test726"
        //let z0 = ((-8.3)+(-y))
        //printfn "%d" <| 726
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-8.3)+(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-8.3)+(-q))
            z2 <== ((-8.3)+(-y))
            wr [I 726; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 727
        !"test727"
        //let z0 = (x*x/8.5)
        //printfn "%d" <| 727
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x*x/8.5)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p*p/8.5)
            z2 <== (x*x/8.5)
            wr [I 727; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 728
        !"test728"
        //let z0 = (-6.6)
        //printfn "%d" <| 728
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 729
        !"test729"
        //let z0 = (-6.6)
        //printfn "%d" <| 729
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 730
        !"test730"
        //let z0 = ((-8.0)+6.5*((-y)+3.1/3.2-(-y)-y)+(0.2)-((-7.3)-(-5.0)-(-y)-8.1-2.0)-(7.0/8.5+3.7-(-y)-(-7.7))-7.7)
        //printfn "%d" <| 730
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-8.0)+6.5*((-y)+3.1/3.2-(-y)-y)+(0.2)-((-7.3)-(-5.0)-(-y)-8.1-2.0)-(7.0/8.5+3.7-(-y)-(-7.7))-7.7)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-8.0)+6.5*((-q)+3.1/3.2-(-q)-q)+(0.2)-((-7.3)-(-5.0)-(-q)-8.1-2.0)-(7.0/8.5+3.7-(-q)-(-7.7))-7.7)
            z2 <== ((-8.0)+6.5*((-y)+3.1/3.2-(-y)-y)+(0.2)-((-7.3)-(-5.0)-(-y)-8.1-2.0)-(7.0/8.5+3.7-(-y)-(-7.7))-7.7)
            wr [I 730; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 731
        !"test731"
        //let z0 = (-6.4)
        //printfn "%d" <| 731
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 732
        !"test732"
        //let z0 = y
        //printfn "%d" <| 732
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 732; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 733
        !"test733"
        //let z0 = (-6.0)
        //printfn "%d" <| 733
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 734
        !"test734"
        //let z0 = (-y)
        //printfn "%d" <| 734
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 734; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 735
        !"test735"
        //let z0 = (((-x)-(7.1+y-(-5.5))-y))
        //printfn "%d" <| 735
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)-(7.1+y-(-5.5))-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)-(7.1+q-(-5.5))-q))
            z2 <== (((-x)-(7.1+y-(-5.5))-y))
            wr [I 735; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 736
        !"test736"
        //let z0 = 2.1
        //printfn "%d" <| 736
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 737
        !"test737"
        //let z0 = (-y)
        //printfn "%d" <| 737
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 737; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 738
        !"test738"
        //let z0 = 5.6
        //printfn "%d" <| 738
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 739
        !"test739"
        //let z0 = y
        //printfn "%d" <| 739
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 739; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 740
        !"test740"
        //let z0 = ((((-y))-(-1.3)+3.7+(x+(-y)+6.4*(-x)+1.1)+x)-(-y))
        //printfn "%d" <| 740
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-y))-(-1.3)+3.7+(x+(-y)+6.4*(-x)+1.1)+x)-(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-q))-(-1.3)+3.7+(p+(-q)+6.4*(-p)+1.1)+p)-(-q))
            z2 <== ((((-y))-(-1.3)+3.7+(x+(-y)+6.4*(-x)+1.1)+x)-(-y))
            wr [I 740; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 741
        !"test741"
        //let z0 = (-3.2)
        //printfn "%d" <| 741
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 742
        !"test742"
        //let z0 = x
        //printfn "%d" <| 742
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 742; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 743
        !"test743"
        //let z0 = y
        //printfn "%d" <| 743
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 743; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 744
        !"test744"
        //let z0 = (-1.7)
        //printfn "%d" <| 744
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 745
        !"test745"
        //let z0 = y
        //printfn "%d" <| 745
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 745; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 746
        !"test746"
        //let z0 = (((-x)/(-3.3)-(-2.8)+x/0.5)*(3.7+2.1+(-x)/(-x)+(-x))/(y))/(-x)+((-4.0))*((-7.5))-(-0.2)
        //printfn "%d" <| 746
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)/(-3.3)-(-2.8)+x/0.5)*(3.7+2.1+(-x)/(-x)+(-x))/(y))/(-x)+((-4.0))*((-7.5))-(-0.2)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)/(-3.3)-(-2.8)+p/0.5)*(3.7+2.1+(-p)/(-p)+(-p))/(q))/(-p)+((-4.0))*((-7.5))-(-0.2)
            z2 <== (((-x)/(-3.3)-(-2.8)+x/0.5)*(3.7+2.1+(-x)/(-x)+(-x))/(y))/(-x)+((-4.0))*((-7.5))-(-0.2)
            wr [I 746; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 747
        !"test747"
        //let z0 = (-1.1)
        //printfn "%d" <| 747
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 748
        !"test748"
        //let z0 = (7.0+3.8+3.2-(-y)+8.5+(-6.3)+0.0+((-y)/1.1+x/(-x)))
        //printfn "%d" <| 748
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.0+3.8+3.2-(-y)+8.5+(-6.3)+0.0+((-y)/1.1+x/(-x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.0+3.8+3.2-(-q)+8.5+(-6.3)+0.0+((-q)/1.1+p/(-p)))
            z2 <== (7.0+3.8+3.2-(-y)+8.5+(-6.3)+0.0+((-y)/1.1+x/(-x)))
            wr [I 748; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 749
        !"test749"
        //let z0 = ((-x)*(x*5.0+(-8.1)/3.3))
        //printfn "%d" <| 749
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)*(x*5.0+(-8.1)/3.3))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)*(p*5.0+(-8.1)/3.3))
            z2 <== ((-x)*(x*5.0+(-8.1)/3.3))
            wr [I 749; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 750
        !"test750"
        //let z0 = (4.2)
        //printfn "%d" <| 750
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 751
        !"test751"
        //let z0 = x
        //printfn "%d" <| 751
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 751; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 752
        !"test752"
        //let z0 = (-5.2)+(-x)+5.1
        //printfn "%d" <| 752
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-5.2)+(-x)+5.1).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-5.2)+(-p)+5.1
            z2 <== (-5.2)+(-x)+5.1
            wr [I 752; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 753
        !"test753"
        //let z0 = (x/(-7.6)-(((-x)/y+8.6))*((5.3)+(7.3*(-x)-0.1*x))+y)
        //printfn "%d" <| 753
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x/(-7.6)-(((-x)/y+8.6))*((5.3)+(7.3*(-x)-0.1*x))+y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p/(-7.6)-(((-p)/q+8.6))*((5.3)+(7.3*(-p)-0.1*p))+q)
            z2 <== (x/(-7.6)-(((-x)/y+8.6))*((5.3)+(7.3*(-x)-0.1*x))+y)
            wr [I 753; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 754
        !"test754"
        //let z0 = 4.8+8.7-0.7/(-x)+(8.4+(-x)-(-0.6)/y)
        //printfn "%d" <| 754
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (4.8+8.7-0.7/(-x)+(8.4+(-x)-(-0.6)/y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== 4.8+8.7-0.7/(-p)+(8.4+(-p)-(-0.6)/q)
            z2 <== 4.8+8.7-0.7/(-x)+(8.4+(-x)-(-0.6)/y)
            wr [I 754; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 755
        !"test755"
        //let z0 = ((2.6*((-0.4)-(-4.3))+(x/y)))
        //printfn "%d" <| 755
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((2.6*((-0.4)-(-4.3))+(x/y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((2.6*((-0.4)-(-4.3))+(p/q)))
            z2 <== ((2.6*((-0.4)-(-4.3))+(x/y)))
            wr [I 755; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 756
        !"test756"
        //let z0 = y
        //printfn "%d" <| 756
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 756; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 757
        !"test757"
        //let z0 = (((-y)/7.1-3.8-(-6.1)+(-y)*(-x)/(-8.1)-(-4.6)*y*5.1+1.1)-2.5+(((-x)*(-y)-x)+(-x)/5.2))
        //printfn "%d" <| 757
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)/7.1-3.8-(-6.1)+(-y)*(-x)/(-8.1)-(-4.6)*y*5.1+1.1)-2.5+(((-x)*(-y)-x)+(-x)/5.2))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)/7.1-3.8-(-6.1)+(-q)*(-p)/(-8.1)-(-4.6)*q*5.1+1.1)-2.5+(((-p)*(-q)-p)+(-p)/5.2))
            z2 <== (((-y)/7.1-3.8-(-6.1)+(-y)*(-x)/(-8.1)-(-4.6)*y*5.1+1.1)-2.5+(((-x)*(-y)-x)+(-x)/5.2))
            wr [I 757; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 758
        !"test758"
        //let z0 = ((((-y))+(6.8/x*7.8-(-x))/(x)-((-y)-(-x)*(-x)+(-x)+(-7.6))-(-4.0)))
        //printfn "%d" <| 758
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-y))+(6.8/x*7.8-(-x))/(x)-((-y)-(-x)*(-x)+(-x)+(-7.6))-(-4.0)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-q))+(6.8/p*7.8-(-p))/(p)-((-q)-(-p)*(-p)+(-p)+(-7.6))-(-4.0)))
            z2 <== ((((-y))+(6.8/x*7.8-(-x))/(x)-((-y)-(-x)*(-x)+(-x)+(-7.6))-(-4.0)))
            wr [I 758; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 759
        !"test759"
        //let z0 = x/x+(-y)
        //printfn "%d" <| 759
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x/x+(-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p/p+(-q)
            z2 <== x/x+(-y)
            wr [I 759; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 760
        !"test760"
        //let z0 = 5.3
        //printfn "%d" <| 760
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 761
        !"test761"
        //let z0 = y
        //printfn "%d" <| 761
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 761; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 762
        !"test762"
        //let z0 = y
        //printfn "%d" <| 762
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 762; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 763
        !"test763"
        //let z0 = (-5.0)
        //printfn "%d" <| 763
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 764
        !"test764"
        //let z0 = (-6.0)
        //printfn "%d" <| 764
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 765
        !"test765"
        //let z0 = 1.2
        //printfn "%d" <| 765
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 766
        !"test766"
        //let z0 = (-y)
        //printfn "%d" <| 766
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 766; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 767
        !"test767"
        //let z0 = 8.6
        //printfn "%d" <| 767
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 768
        !"test768"
        //let z0 = (x)
        //printfn "%d" <| 768
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p)
            z2 <== (x)
            wr [I 768; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 769
        !"test769"
        //let z0 = (x-(x+3.0*1.6+(y))-7.5+6.4)
        //printfn "%d" <| 769
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x-(x+3.0*1.6+(y))-7.5+6.4)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p-(p+3.0*1.6+(q))-7.5+6.4)
            z2 <== (x-(x+3.0*1.6+(y))-7.5+6.4)
            wr [I 769; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 770
        !"test770"
        //let z0 = (x-x-(((-x)-(-6.5)-(-4.0)-(-x)/8.2))/((-5.0)*5.3-((-y)-8.2))/(-x))
        //printfn "%d" <| 770
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x-x-(((-x)-(-6.5)-(-4.0)-(-x)/8.2))/((-5.0)*5.3-((-y)-8.2))/(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p-p-(((-p)-(-6.5)-(-4.0)-(-p)/8.2))/((-5.0)*5.3-((-q)-8.2))/(-p))
            z2 <== (x-x-(((-x)-(-6.5)-(-4.0)-(-x)/8.2))/((-5.0)*5.3-((-y)-8.2))/(-x))
            wr [I 770; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 771
        !"test771"
        //let z0 = (((-2.7))+((-5.2))+(-3.1)-(((-0.6)*5.1/y/5.4)*((-x)+6.7*(-4.3))-x))
        //printfn "%d" <| 771
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-2.7))+((-5.2))+(-3.1)-(((-0.6)*5.1/y/5.4)*((-x)+6.7*(-4.3))-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-2.7))+((-5.2))+(-3.1)-(((-0.6)*5.1/q/5.4)*((-p)+6.7*(-4.3))-p))
            z2 <== (((-2.7))+((-5.2))+(-3.1)-(((-0.6)*5.1/y/5.4)*((-x)+6.7*(-4.3))-x))
            wr [I 771; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 772
        !"test772"
        //let z0 = (y*((x-y+y/(-0.1))-(8.0/(-y)-x+(-y))*(-2.7)+(-6.6)))
        //printfn "%d" <| 772
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y*((x-y+y/(-0.1))-(8.0/(-y)-x+(-y))*(-2.7)+(-6.6)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q*((p-q+q/(-0.1))-(8.0/(-q)-p+(-q))*(-2.7)+(-6.6)))
            z2 <== (y*((x-y+y/(-0.1))-(8.0/(-y)-x+(-y))*(-2.7)+(-6.6)))
            wr [I 772; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 773
        !"test773"
        //let z0 = y+(-x)*(-8.1)
        //printfn "%d" <| 773
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y+(-x)*(-8.1)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q+(-p)*(-8.1)
            z2 <== y+(-x)*(-8.1)
            wr [I 773; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 774
        !"test774"
        //let z0 = y
        //printfn "%d" <| 774
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 774; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 775
        !"test775"
        //let z0 = ((-x)+(-1.2)-(-x)+(2.1-0.8+(-3.6)+y+((-y)*(-2.1))/(7.2))/(y))
        //printfn "%d" <| 775
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+(-1.2)-(-x)+(2.1-0.8+(-3.6)+y+((-y)*(-2.1))/(7.2))/(y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+(-1.2)-(-p)+(2.1-0.8+(-3.6)+q+((-q)*(-2.1))/(7.2))/(q))
            z2 <== ((-x)+(-1.2)-(-x)+(2.1-0.8+(-3.6)+y+((-y)*(-2.1))/(7.2))/(y))
            wr [I 775; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 776
        !"test776"
        //let z0 = ((-x)-4.4)
        //printfn "%d" <| 776
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)-4.4)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)-4.4)
            z2 <== ((-x)-4.4)
            wr [I 776; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 777
        !"test777"
        //let z0 = (-8.8)
        //printfn "%d" <| 777
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 778
        !"test778"
        //let z0 = (-7.4)
        //printfn "%d" <| 778
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 779
        !"test779"
        //let z0 = (6.5+(-8.3)/(-1.0))
        //printfn "%d" <| 779
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 780
        !"test780"
        //let z0 = (-5.2)
        //printfn "%d" <| 780
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 781
        !"test781"
        //let z0 = (-7.1)
        //printfn "%d" <| 781
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 782
        !"test782"
        //let z0 = ((4.3-(7.0*5.5*(-x)/(-8.2)-3.1))/(-5.7)/(-y))
        //printfn "%d" <| 782
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((4.3-(7.0*5.5*(-x)/(-8.2)-3.1))/(-5.7)/(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((4.3-(7.0*5.5*(-p)/(-8.2)-3.1))/(-5.7)/(-q))
            z2 <== ((4.3-(7.0*5.5*(-x)/(-8.2)-3.1))/(-5.7)/(-y))
            wr [I 782; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 783
        !"test783"
        //let z0 = (-x)
        //printfn "%d" <| 783
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 783; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 784
        !"test784"
        //let z0 = (0.5+(0.5+((-y)*x/0.6))-(x)-(-x)-(-x))
        //printfn "%d" <| 784
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((0.5+(0.5+((-y)*x/0.6))-(x)-(-x)-(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (0.5+(0.5+((-q)*p/0.6))-(p)-(-p)-(-p))
            z2 <== (0.5+(0.5+((-y)*x/0.6))-(x)-(-x)-(-x))
            wr [I 784; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 785
        !"test785"
        //let z0 = (-x)
        //printfn "%d" <| 785
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 785; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 786
        !"test786"
        //let z0 = (x)
        //printfn "%d" <| 786
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p)
            z2 <== (x)
            wr [I 786; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 787
        !"test787"
        //let z0 = (y*y-(2.4-y)-(x-(-y)/y)/((2.5)+(x)-y/x+(y-(-2.8))))
        //printfn "%d" <| 787
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y*y-(2.4-y)-(x-(-y)/y)/((2.5)+(x)-y/x+(y-(-2.8))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q*q-(2.4-q)-(p-(-q)/q)/((2.5)+(p)-q/p+(q-(-2.8))))
            z2 <== (y*y-(2.4-y)-(x-(-y)/y)/((2.5)+(x)-y/x+(y-(-2.8))))
            wr [I 787; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 788
        !"test788"
        //let z0 = 3.1
        //printfn "%d" <| 788
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 789
        !"test789"
        //let z0 = (-x)
        //printfn "%d" <| 789
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 789; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 790
        !"test790"
        //let z0 = (-1.1)
        //printfn "%d" <| 790
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 791
        !"test791"
        //let z0 = ((-8.1)/y/((-y)/y)-((y*x)-(-1.0)-(-4.1)*(-x))/(6.5+6.6/x))
        //printfn "%d" <| 791
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-8.1)/y/((-y)/y)-((y*x)-(-1.0)-(-4.1)*(-x))/(6.5+6.6/x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-8.1)/q/((-q)/q)-((q*p)-(-1.0)-(-4.1)*(-p))/(6.5+6.6/p))
            z2 <== ((-8.1)/y/((-y)/y)-((y*x)-(-1.0)-(-4.1)*(-x))/(6.5+6.6/x))
            wr [I 791; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 792
        !"test792"
        //let z0 = x
        //printfn "%d" <| 792
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 792; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 793
        !"test793"
        //let z0 = (7.2/6.6/(-4.3)/(-5.7)*x)/(y)+((-8.7)/x)/(-1.6)+0.0-(((-8.1)+x-(-6.2)-y)/(-x)+x)+(-y)+(-y)/(-8.2)-(-y)/y-((-x)+0.7+(-y)*(-0.7)*5.3)/(-x)
        //printfn "%d" <| 793
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.2/6.6/(-4.3)/(-5.7)*x)/(y)+((-8.7)/x)/(-1.6)+0.0-(((-8.1)+x-(-6.2)-y)/(-x)+x)+(-y)+(-y)/(-8.2)-(-y)/y-((-x)+0.7+(-y)*(-0.7)*5.3)/(-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.2/6.6/(-4.3)/(-5.7)*p)/(q)+((-8.7)/p)/(-1.6)+0.0-(((-8.1)+p-(-6.2)-q)/(-p)+p)+(-q)+(-q)/(-8.2)-(-q)/q-((-p)+0.7+(-q)*(-0.7)*5.3)/(-p)
            z2 <== (7.2/6.6/(-4.3)/(-5.7)*x)/(y)+((-8.7)/x)/(-1.6)+0.0-(((-8.1)+x-(-6.2)-y)/(-x)+x)+(-y)+(-y)/(-8.2)-(-y)/y-((-x)+0.7+(-y)*(-0.7)*5.3)/(-x)
            wr [I 793; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 794
        !"test794"
        //let z0 = (-5.6)
        //printfn "%d" <| 794
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 795
        !"test795"
        //let z0 = (-7.2)
        //printfn "%d" <| 795
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 796
        !"test796"
        //let z0 = (((-y)-y)/y*((-6.6)*3.6*x+(-0.7))/(4.0+((-7.1)/y-2.7)-(x)-y+(7.5-(-3.3)-(-2.6)*x-y))+(y+((-6.5)*7.0*(-x))))
        //printfn "%d" <| 796
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)-y)/y*((-6.6)*3.6*x+(-0.7))/(4.0+((-7.1)/y-2.7)-(x)-y+(7.5-(-3.3)-(-2.6)*x-y))+(y+((-6.5)*7.0*(-x))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)-q)/q*((-6.6)*3.6*p+(-0.7))/(4.0+((-7.1)/q-2.7)-(p)-q+(7.5-(-3.3)-(-2.6)*p-q))+(q+((-6.5)*7.0*(-p))))
            z2 <== (((-y)-y)/y*((-6.6)*3.6*x+(-0.7))/(4.0+((-7.1)/y-2.7)-(x)-y+(7.5-(-3.3)-(-2.6)*x-y))+(y+((-6.5)*7.0*(-x))))
            wr [I 796; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 797
        !"test797"
        //let z0 = y
        //printfn "%d" <| 797
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 797; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 798
        !"test798"
        //let z0 = (-3.4)
        //printfn "%d" <| 798
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 799
        !"test799"
        //let z0 = (-8.5)/x
        //printfn "%d" <| 799
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-8.5)/x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-8.5)/p
            z2 <== (-8.5)/x
            wr [I 799; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 800
        !"test800"
        //let z0 = ((x)*(-3.5)+x+((x-(-3.4)-(-6.1)+1.7)-((-y))/1.4+(x*8.2)/(-y))+(-8.4))
        //printfn "%d" <| 800
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x)*(-3.5)+x+((x-(-3.4)-(-6.1)+1.7)-((-y))/1.4+(x*8.2)/(-y))+(-8.4))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p)*(-3.5)+p+((p-(-3.4)-(-6.1)+1.7)-((-q))/1.4+(p*8.2)/(-q))+(-8.4))
            z2 <== ((x)*(-3.5)+x+((x-(-3.4)-(-6.1)+1.7)-((-y))/1.4+(x*8.2)/(-y))+(-8.4))
            wr [I 800; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 801
        !"test801"
        //let z0 = ((-5.2)+y+((-4.8)/((-7.3)))*((-0.7)+x*6.7*(-y)+((-x)*7.0+(-3.0)+6.6)))
        //printfn "%d" <| 801
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-5.2)+y+((-4.8)/((-7.3)))*((-0.7)+x*6.7*(-y)+((-x)*7.0+(-3.0)+6.6)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-5.2)+q+((-4.8)/((-7.3)))*((-0.7)+p*6.7*(-q)+((-p)*7.0+(-3.0)+6.6)))
            z2 <== ((-5.2)+y+((-4.8)/((-7.3)))*((-0.7)+x*6.7*(-y)+((-x)*7.0+(-3.0)+6.6)))
            wr [I 801; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 802
        !"test802"
        //let z0 = (((-3.2)-(-x)-y/7.0-(1.8+x-(-1.5)-4.8)*((-x)-x*x/(-0.3))*3.3)+3.6/(((-y)-(-4.6)/y+4.0*4.5)/x/x)/(8.8*((-x)-x*y+2.2)/(-4.4)/((-x)*(-x))))
        //printfn "%d" <| 802
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-3.2)-(-x)-y/7.0-(1.8+x-(-1.5)-4.8)*((-x)-x*x/(-0.3))*3.3)+3.6/(((-y)-(-4.6)/y+4.0*4.5)/x/x)/(8.8*((-x)-x*y+2.2)/(-4.4)/((-x)*(-x))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-3.2)-(-p)-q/7.0-(1.8+p-(-1.5)-4.8)*((-p)-p*p/(-0.3))*3.3)+3.6/(((-q)-(-4.6)/q+4.0*4.5)/p/p)/(8.8*((-p)-p*q+2.2)/(-4.4)/((-p)*(-p))))
            z2 <== (((-3.2)-(-x)-y/7.0-(1.8+x-(-1.5)-4.8)*((-x)-x*x/(-0.3))*3.3)+3.6/(((-y)-(-4.6)/y+4.0*4.5)/x/x)/(8.8*((-x)-x*y+2.2)/(-4.4)/((-x)*(-x))))
            wr [I 802; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 803
        !"test803"
        //let z0 = ((y/(-x)/y*(-y)*(-x)-(-y)-(-0.1))*(((-1.3))-(-y)/7.6/(-7.4)/(-y)))
        //printfn "%d" <| 803
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y/(-x)/y*(-y)*(-x)-(-y)-(-0.1))*(((-1.3))-(-y)/7.6/(-7.4)/(-y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q/(-p)/q*(-q)*(-p)-(-q)-(-0.1))*(((-1.3))-(-q)/7.6/(-7.4)/(-q)))
            z2 <== ((y/(-x)/y*(-y)*(-x)-(-y)-(-0.1))*(((-1.3))-(-y)/7.6/(-7.4)/(-y)))
            wr [I 803; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 804
        !"test804"
        //let z0 = (-8.5)
        //printfn "%d" <| 804
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 805
        !"test805"
        //let z0 = ((((-6.5)*(-y)*x/(-8.1)))+1.8-((0.0*(-2.3)+6.5/(-y))/(-y)*7.6)-(-y)-(7.1))
        //printfn "%d" <| 805
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-6.5)*(-y)*x/(-8.1)))+1.8-((0.0*(-2.3)+6.5/(-y))/(-y)*7.6)-(-y)-(7.1))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-6.5)*(-q)*p/(-8.1)))+1.8-((0.0*(-2.3)+6.5/(-q))/(-q)*7.6)-(-q)-(7.1))
            z2 <== ((((-6.5)*(-y)*x/(-8.1)))+1.8-((0.0*(-2.3)+6.5/(-y))/(-y)*7.6)-(-y)-(7.1))
            wr [I 805; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 806
        !"test806"
        //let z0 = y
        //printfn "%d" <| 806
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 806; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 807
        !"test807"
        //let z0 = y
        //printfn "%d" <| 807
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 807; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 808
        !"test808"
        //let z0 = (-x)
        //printfn "%d" <| 808
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 808; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 809
        !"test809"
        //let z0 = ((-x)*(-x)/(-3.4)*(-y)-y+8.5+2.8/(-4.2)+((-2.2)/7.8-(-x))/6.8)+((-x)/(-x)+(-y))-(3.6/(-x)/3.7+y/(-5.2))-(-5.3)+5.8
        //printfn "%d" <| 809
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)*(-x)/(-3.4)*(-y)-y+8.5+2.8/(-4.2)+((-2.2)/7.8-(-x))/6.8)+((-x)/(-x)+(-y))-(3.6/(-x)/3.7+y/(-5.2))-(-5.3)+5.8).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)*(-p)/(-3.4)*(-q)-q+8.5+2.8/(-4.2)+((-2.2)/7.8-(-p))/6.8)+((-p)/(-p)+(-q))-(3.6/(-p)/3.7+q/(-5.2))-(-5.3)+5.8
            z2 <== ((-x)*(-x)/(-3.4)*(-y)-y+8.5+2.8/(-4.2)+((-2.2)/7.8-(-x))/6.8)+((-x)/(-x)+(-y))-(3.6/(-x)/3.7+y/(-5.2))-(-5.3)+5.8
            wr [I 809; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 810
        !"test810"
        //let z0 = 7.1
        //printfn "%d" <| 810
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 811
        !"test811"
        //let z0 = ((-x)*((-x))*(8.4)-((5.8-x+(-x)+(-4.7))/((-8.7)/2.5+3.3*7.0)-8.7)+(-6.7))
        //printfn "%d" <| 811
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)*((-x))*(8.4)-((5.8-x+(-x)+(-4.7))/((-8.7)/2.5+3.3*7.0)-8.7)+(-6.7))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)*((-p))*(8.4)-((5.8-p+(-p)+(-4.7))/((-8.7)/2.5+3.3*7.0)-8.7)+(-6.7))
            z2 <== ((-x)*((-x))*(8.4)-((5.8-x+(-x)+(-4.7))/((-8.7)/2.5+3.3*7.0)-8.7)+(-6.7))
            wr [I 811; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 812
        !"test812"
        //let z0 = ((-0.1)*(y+(-x)/(-0.0)/((-3.7)+3.7)/((-y)+(-x)-(-1.5)*1.6+y)*(5.2-(-4.5)+0.7))/((-x)/7.1+(-8.3)*(-4.2)+(-y)-((-7.7)+(-y)*x)*(7.4/(-5.1))+(-x)))
        //printfn "%d" <| 812
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.1)*(y+(-x)/(-0.0)/((-3.7)+3.7)/((-y)+(-x)-(-1.5)*1.6+y)*(5.2-(-4.5)+0.7))/((-x)/7.1+(-8.3)*(-4.2)+(-y)-((-7.7)+(-y)*x)*(7.4/(-5.1))+(-x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.1)*(q+(-p)/(-0.0)/((-3.7)+3.7)/((-q)+(-p)-(-1.5)*1.6+q)*(5.2-(-4.5)+0.7))/((-p)/7.1+(-8.3)*(-4.2)+(-q)-((-7.7)+(-q)*p)*(7.4/(-5.1))+(-p)))
            z2 <== ((-0.1)*(y+(-x)/(-0.0)/((-3.7)+3.7)/((-y)+(-x)-(-1.5)*1.6+y)*(5.2-(-4.5)+0.7))/((-x)/7.1+(-8.3)*(-4.2)+(-y)-((-7.7)+(-y)*x)*(7.4/(-5.1))+(-x)))
            wr [I 812; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 813
        !"test813"
        //let z0 = (0.3)
        //printfn "%d" <| 813
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 814
        !"test814"
        //let z0 = (-1.7)
        //printfn "%d" <| 814
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 815
        !"test815"
        //let z0 = (-y)
        //printfn "%d" <| 815
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 815; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 816
        !"test816"
        //let z0 = ((y)+(y+y/(y+0.2+6.5+x)*7.0-(-x)-4.5/(-x)-6.8)/(3.6/(-7.0)+y)/4.8/y*(-x)*((-8.5))-8.2)
        //printfn "%d" <| 816
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y)+(y+y/(y+0.2+6.5+x)*7.0-(-x)-4.5/(-x)-6.8)/(3.6/(-7.0)+y)/4.8/y*(-x)*((-8.5))-8.2)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q)+(q+q/(q+0.2+6.5+p)*7.0-(-p)-4.5/(-p)-6.8)/(3.6/(-7.0)+q)/4.8/q*(-p)*((-8.5))-8.2)
            z2 <== ((y)+(y+y/(y+0.2+6.5+x)*7.0-(-x)-4.5/(-x)-6.8)/(3.6/(-7.0)+y)/4.8/y*(-x)*((-8.5))-8.2)
            wr [I 816; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 817
        !"test817"
        //let z0 = (-x)
        //printfn "%d" <| 817
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 817; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 818
        !"test818"
        //let z0 = ((1.6+x/(-x)+(-x)+(-0.4))*(-x)*(8.0)+6.3/((-6.6)))
        //printfn "%d" <| 818
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((1.6+x/(-x)+(-x)+(-0.4))*(-x)*(8.0)+6.3/((-6.6)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((1.6+p/(-p)+(-p)+(-0.4))*(-p)*(8.0)+6.3/((-6.6)))
            z2 <== ((1.6+x/(-x)+(-x)+(-0.4))*(-x)*(8.0)+6.3/((-6.6)))
            wr [I 818; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 819
        !"test819"
        //let z0 = (-x)
        //printfn "%d" <| 819
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 819; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 820
        !"test820"
        //let z0 = (((-7.3)+y*(-x)/(-x)+x)/(-5.6))-(y-(-y)*(-5.2)/((-y)-y)*(-3.5)/(-2.1)/(-y)*4.0*(-1.3)*y)+(3.7/(-y))
        //printfn "%d" <| 820
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-7.3)+y*(-x)/(-x)+x)/(-5.6))-(y-(-y)*(-5.2)/((-y)-y)*(-3.5)/(-2.1)/(-y)*4.0*(-1.3)*y)+(3.7/(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-7.3)+q*(-p)/(-p)+p)/(-5.6))-(q-(-q)*(-5.2)/((-q)-q)*(-3.5)/(-2.1)/(-q)*4.0*(-1.3)*q)+(3.7/(-q))
            z2 <== (((-7.3)+y*(-x)/(-x)+x)/(-5.6))-(y-(-y)*(-5.2)/((-y)-y)*(-3.5)/(-2.1)/(-y)*4.0*(-1.3)*y)+(3.7/(-y))
            wr [I 820; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 821
        !"test821"
        //let z0 = ((-x)+(0.1+(-7.3)*6.4*(-4.7)*(-y)*((-4.6))-(-3.8))*((-x)+3.0+1.6-(-7.1)*(-3.1)+x/(-y)/5.1/(-8.1))/(((-1.5)+2.7-5.2)*((-3.1)+2.6*(-2.3))/(-y)/(-2.6)/(-5.3)))
        //printfn "%d" <| 821
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)+(0.1+(-7.3)*6.4*(-4.7)*(-y)*((-4.6))-(-3.8))*((-x)+3.0+1.6-(-7.1)*(-3.1)+x/(-y)/5.1/(-8.1))/(((-1.5)+2.7-5.2)*((-3.1)+2.6*(-2.3))/(-y)/(-2.6)/(-5.3)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)+(0.1+(-7.3)*6.4*(-4.7)*(-q)*((-4.6))-(-3.8))*((-p)+3.0+1.6-(-7.1)*(-3.1)+p/(-q)/5.1/(-8.1))/(((-1.5)+2.7-5.2)*((-3.1)+2.6*(-2.3))/(-q)/(-2.6)/(-5.3)))
            z2 <== ((-x)+(0.1+(-7.3)*6.4*(-4.7)*(-y)*((-4.6))-(-3.8))*((-x)+3.0+1.6-(-7.1)*(-3.1)+x/(-y)/5.1/(-8.1))/(((-1.5)+2.7-5.2)*((-3.1)+2.6*(-2.3))/(-y)/(-2.6)/(-5.3)))
            wr [I 821; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 822
        !"test822"
        //let z0 = 2.7
        //printfn "%d" <| 822
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 823
        !"test823"
        //let z0 = (((y)*((-7.4)*(-7.7)+(-x)+y)+0.4-(y+(-x)-(-8.8)/y))-((4.6/x/x*y-(-y))*(-5.6)-(y-x/6.3*4.0)+((-y)/(-x)*(-2.2)/6.1-5.7)-((-1.5)*(-5.0)-(-5.3))))
        //printfn "%d" <| 823
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((y)*((-7.4)*(-7.7)+(-x)+y)+0.4-(y+(-x)-(-8.8)/y))-((4.6/x/x*y-(-y))*(-5.6)-(y-x/6.3*4.0)+((-y)/(-x)*(-2.2)/6.1-5.7)-((-1.5)*(-5.0)-(-5.3))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((q)*((-7.4)*(-7.7)+(-p)+q)+0.4-(q+(-p)-(-8.8)/q))-((4.6/p/p*q-(-q))*(-5.6)-(q-p/6.3*4.0)+((-q)/(-p)*(-2.2)/6.1-5.7)-((-1.5)*(-5.0)-(-5.3))))
            z2 <== (((y)*((-7.4)*(-7.7)+(-x)+y)+0.4-(y+(-x)-(-8.8)/y))-((4.6/x/x*y-(-y))*(-5.6)-(y-x/6.3*4.0)+((-y)/(-x)*(-2.2)/6.1-5.7)-((-1.5)*(-5.0)-(-5.3))))
            wr [I 823; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 824
        !"test824"
        //let z0 = (-y)
        //printfn "%d" <| 824
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 824; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 825
        !"test825"
        //let z0 = (-4.1)
        //printfn "%d" <| 825
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 826
        !"test826"
        //let z0 = ((0.8*(-y))*6.7/7.4/((-8.1)-(-y)+(-y)*y/0.7/2.5+4.2+3.7-(-1.4)*(-2.6)/y-((-7.4)-(-x)*4.6)))
        //printfn "%d" <| 826
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((0.8*(-y))*6.7/7.4/((-8.1)-(-y)+(-y)*y/0.7/2.5+4.2+3.7-(-1.4)*(-2.6)/y-((-7.4)-(-x)*4.6)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((0.8*(-q))*6.7/7.4/((-8.1)-(-q)+(-q)*q/0.7/2.5+4.2+3.7-(-1.4)*(-2.6)/q-((-7.4)-(-p)*4.6)))
            z2 <== ((0.8*(-y))*6.7/7.4/((-8.1)-(-y)+(-y)*y/0.7/2.5+4.2+3.7-(-1.4)*(-2.6)/y-((-7.4)-(-x)*4.6)))
            wr [I 826; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 827
        !"test827"
        //let z0 = y+(5.3*x+(-x)-8.8)*(4.4+(-x))*(-y)*x
        //printfn "%d" <| 827
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y+(5.3*x+(-x)-8.8)*(4.4+(-x))*(-y)*x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q+(5.3*p+(-p)-8.8)*(4.4+(-p))*(-q)*p
            z2 <== y+(5.3*x+(-x)-8.8)*(4.4+(-x))*(-y)*x
            wr [I 827; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 828
        !"test828"
        //let z0 = x
        //printfn "%d" <| 828
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 828; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 829
        !"test829"
        //let z0 = ((-5.3)-3.5+(-x)*((-y)/(-1.8)+(-x)-(-1.2)-(-y)/(-1.5))+(-0.5)-(((-x)-(-x)-y*1.2)))
        //printfn "%d" <| 829
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-5.3)-3.5+(-x)*((-y)/(-1.8)+(-x)-(-1.2)-(-y)/(-1.5))+(-0.5)-(((-x)-(-x)-y*1.2)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-5.3)-3.5+(-p)*((-q)/(-1.8)+(-p)-(-1.2)-(-q)/(-1.5))+(-0.5)-(((-p)-(-p)-q*1.2)))
            z2 <== ((-5.3)-3.5+(-x)*((-y)/(-1.8)+(-x)-(-1.2)-(-y)/(-1.5))+(-0.5)-(((-x)-(-x)-y*1.2)))
            wr [I 829; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 830
        !"test830"
        //let z0 = (((1.0*(-x)+x+(-y))/(-5.4))*(-2.6))
        //printfn "%d" <| 830
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((1.0*(-x)+x+(-y))/(-5.4))*(-2.6))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((1.0*(-p)+p+(-q))/(-5.4))*(-2.6))
            z2 <== (((1.0*(-x)+x+(-y))/(-5.4))*(-2.6))
            wr [I 830; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 831
        !"test831"
        //let z0 = x
        //printfn "%d" <| 831
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 831; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 832
        !"test832"
        //let z0 = (-y)
        //printfn "%d" <| 832
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 832; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 833
        !"test833"
        //let z0 = ((((-x))-(-7.2)/(-x)+((-x)*(-x)))+y-(((-x)+(-x)*(-y)/y)+(-x)*(2.5-2.1+3.3)+((-x)-2.4/(-2.8)+(-x)/3.3)+((-x)*(-3.1)/y+x))+(-x)+(y/((-5.5)*(-y)/1.0)))
        //printfn "%d" <| 833
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-x))-(-7.2)/(-x)+((-x)*(-x)))+y-(((-x)+(-x)*(-y)/y)+(-x)*(2.5-2.1+3.3)+((-x)-2.4/(-2.8)+(-x)/3.3)+((-x)*(-3.1)/y+x))+(-x)+(y/((-5.5)*(-y)/1.0)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-p))-(-7.2)/(-p)+((-p)*(-p)))+q-(((-p)+(-p)*(-q)/q)+(-p)*(2.5-2.1+3.3)+((-p)-2.4/(-2.8)+(-p)/3.3)+((-p)*(-3.1)/q+p))+(-p)+(q/((-5.5)*(-q)/1.0)))
            z2 <== ((((-x))-(-7.2)/(-x)+((-x)*(-x)))+y-(((-x)+(-x)*(-y)/y)+(-x)*(2.5-2.1+3.3)+((-x)-2.4/(-2.8)+(-x)/3.3)+((-x)*(-3.1)/y+x))+(-x)+(y/((-5.5)*(-y)/1.0)))
            wr [I 833; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 834
        !"test834"
        //let z0 = 7.2
        //printfn "%d" <| 834
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 835
        !"test835"
        //let z0 = 7.4
        //printfn "%d" <| 835
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 836
        !"test836"
        //let z0 = ((0.0+4.0)-(((-8.1)/6.2-x)-(2.5/y-(-2.2)-(-3.4)-(-x))+(-4.2)/0.6)/x)
        //printfn "%d" <| 836
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((0.0+4.0)-(((-8.1)/6.2-x)-(2.5/y-(-2.2)-(-3.4)-(-x))+(-4.2)/0.6)/x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((0.0+4.0)-(((-8.1)/6.2-p)-(2.5/q-(-2.2)-(-3.4)-(-p))+(-4.2)/0.6)/p)
            z2 <== ((0.0+4.0)-(((-8.1)/6.2-x)-(2.5/y-(-2.2)-(-3.4)-(-x))+(-4.2)/0.6)/x)
            wr [I 836; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 837
        !"test837"
        //let z0 = (-0.2)
        //printfn "%d" <| 837
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 838
        !"test838"
        //let z0 = (((y)+(x)-y-((-5.0)-(-y)+7.2)-y))
        //printfn "%d" <| 838
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((y)+(x)-y-((-5.0)-(-y)+7.2)-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((q)+(p)-q-((-5.0)-(-q)+7.2)-q))
            z2 <== (((y)+(x)-y-((-5.0)-(-y)+7.2)-y))
            wr [I 838; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 839
        !"test839"
        //let z0 = y
        //printfn "%d" <| 839
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 839; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 840
        !"test840"
        //let z0 = x
        //printfn "%d" <| 840
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 840; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 841
        !"test841"
        //let z0 = 4.0
        //printfn "%d" <| 841
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 842
        !"test842"
        //let z0 = 8.1
        //printfn "%d" <| 842
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 843
        !"test843"
        //let z0 = (2.0-7.8-(0.5)-(((-y)-7.2)+x/(-7.5)-((-8.8)*(-4.4)+(-x)+(-3.7)-2.8)))
        //printfn "%d" <| 843
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((2.0-7.8-(0.5)-(((-y)-7.2)+x/(-7.5)-((-8.8)*(-4.4)+(-x)+(-3.7)-2.8)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (2.0-7.8-(0.5)-(((-q)-7.2)+p/(-7.5)-((-8.8)*(-4.4)+(-p)+(-3.7)-2.8)))
            z2 <== (2.0-7.8-(0.5)-(((-y)-7.2)+x/(-7.5)-((-8.8)*(-4.4)+(-x)+(-3.7)-2.8)))
            wr [I 843; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 844
        !"test844"
        //let z0 = ((-y)-(((-6.5)/(-7.2)*5.6)-((-0.0))*(-1.7)/(-y)*y+(1.4/5.1/(-7.0)*(-3.1))))
        //printfn "%d" <| 844
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)-(((-6.5)/(-7.2)*5.6)-((-0.0))*(-1.7)/(-y)*y+(1.4/5.1/(-7.0)*(-3.1))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)-(((-6.5)/(-7.2)*5.6)-((-0.0))*(-1.7)/(-q)*q+(1.4/5.1/(-7.0)*(-3.1))))
            z2 <== ((-y)-(((-6.5)/(-7.2)*5.6)-((-0.0))*(-1.7)/(-y)*y+(1.4/5.1/(-7.0)*(-3.1))))
            wr [I 844; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 845
        !"test845"
        //let z0 = y
        //printfn "%d" <| 845
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 845; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 846
        !"test846"
        //let z0 = 1.8
        //printfn "%d" <| 846
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 847
        !"test847"
        //let z0 = (-1.5)
        //printfn "%d" <| 847
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 848
        !"test848"
        //let z0 = (5.6+((-x))+(((-2.8))*y+((-5.4)/(-y)+x-(-1.5))/8.3)+(((-x)*(-x)*(-0.6)+(-x))-((-4.1)+(-y)+7.2-(-1.5))+(-y)+((-y)/7.7))-x)
        //printfn "%d" <| 848
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((5.6+((-x))+(((-2.8))*y+((-5.4)/(-y)+x-(-1.5))/8.3)+(((-x)*(-x)*(-0.6)+(-x))-((-4.1)+(-y)+7.2-(-1.5))+(-y)+((-y)/7.7))-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (5.6+((-p))+(((-2.8))*q+((-5.4)/(-q)+p-(-1.5))/8.3)+(((-p)*(-p)*(-0.6)+(-p))-((-4.1)+(-q)+7.2-(-1.5))+(-q)+((-q)/7.7))-p)
            z2 <== (5.6+((-x))+(((-2.8))*y+((-5.4)/(-y)+x-(-1.5))/8.3)+(((-x)*(-x)*(-0.6)+(-x))-((-4.1)+(-y)+7.2-(-1.5))+(-y)+((-y)/7.7))-x)
            wr [I 848; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 849
        !"test849"
        //let z0 = (-1.7)
        //printfn "%d" <| 849
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 850
        !"test850"
        //let z0 = 1.7
        //printfn "%d" <| 850
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 851
        !"test851"
        //let z0 = ((2.7/(1.5*y/4.0*y-(-x))/y+(y/5.6*(-1.7)+x-x)-(-3.2))*y/(-y)*(8.7*((-0.4)-6.3+y))/5.1/8.4*(x/y+7.1+y-(-y))/(7.7-(-8.1)-y-(-2.0)-7.6)+((-4.3)/(-y)-x))
        //printfn "%d" <| 851
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((2.7/(1.5*y/4.0*y-(-x))/y+(y/5.6*(-1.7)+x-x)-(-3.2))*y/(-y)*(8.7*((-0.4)-6.3+y))/5.1/8.4*(x/y+7.1+y-(-y))/(7.7-(-8.1)-y-(-2.0)-7.6)+((-4.3)/(-y)-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((2.7/(1.5*q/4.0*q-(-p))/q+(q/5.6*(-1.7)+p-p)-(-3.2))*q/(-q)*(8.7*((-0.4)-6.3+q))/5.1/8.4*(p/q+7.1+q-(-q))/(7.7-(-8.1)-q-(-2.0)-7.6)+((-4.3)/(-q)-p))
            z2 <== ((2.7/(1.5*y/4.0*y-(-x))/y+(y/5.6*(-1.7)+x-x)-(-3.2))*y/(-y)*(8.7*((-0.4)-6.3+y))/5.1/8.4*(x/y+7.1+y-(-y))/(7.7-(-8.1)-y-(-2.0)-7.6)+((-4.3)/(-y)-x))
            wr [I 851; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 852
        !"test852"
        //let z0 = (y-(((-x)))-7.6+x)
        //printfn "%d" <| 852
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y-(((-x)))-7.6+x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q-(((-p)))-7.6+p)
            z2 <== (y-(((-x)))-7.6+x)
            wr [I 852; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 853
        !"test853"
        //let z0 = (((-0.2))-(x-5.8*(-8.0)*(-x)*(-x)*(-x))+(y-x-(-2.6)*y*y-(y-y+(-8.0)/(-1.4)+(-y))+(-6.2))-(-8.7))
        //printfn "%d" <| 853
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-0.2))-(x-5.8*(-8.0)*(-x)*(-x)*(-x))+(y-x-(-2.6)*y*y-(y-y+(-8.0)/(-1.4)+(-y))+(-6.2))-(-8.7))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-0.2))-(p-5.8*(-8.0)*(-p)*(-p)*(-p))+(q-p-(-2.6)*q*q-(q-q+(-8.0)/(-1.4)+(-q))+(-6.2))-(-8.7))
            z2 <== (((-0.2))-(x-5.8*(-8.0)*(-x)*(-x)*(-x))+(y-x-(-2.6)*y*y-(y-y+(-8.0)/(-1.4)+(-y))+(-6.2))-(-8.7))
            wr [I 853; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 854
        !"test854"
        //let z0 = ((-7.8)/4.6/(x)-0.1+5.4+((-0.3)-y/8.5-(-1.7)/(-8.7)))
        //printfn "%d" <| 854
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.8)/4.6/(x)-0.1+5.4+((-0.3)-y/8.5-(-1.7)/(-8.7)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.8)/4.6/(p)-0.1+5.4+((-0.3)-q/8.5-(-1.7)/(-8.7)))
            z2 <== ((-7.8)/4.6/(x)-0.1+5.4+((-0.3)-y/8.5-(-1.7)/(-8.7)))
            wr [I 854; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 855
        !"test855"
        //let z0 = (-x)
        //printfn "%d" <| 855
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 855; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 856
        !"test856"
        //let z0 = ((-7.4)/y/((-4.8)+(-8.0)+(x*8.2/y))-(-4.3)-5.3)
        //printfn "%d" <| 856
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.4)/y/((-4.8)+(-8.0)+(x*8.2/y))-(-4.3)-5.3)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.4)/q/((-4.8)+(-8.0)+(p*8.2/q))-(-4.3)-5.3)
            z2 <== ((-7.4)/y/((-4.8)+(-8.0)+(x*8.2/y))-(-4.3)-5.3)
            wr [I 856; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 857
        !"test857"
        //let z0 = ((-y)-(-x))
        //printfn "%d" <| 857
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)-(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)-(-p))
            z2 <== ((-y)-(-x))
            wr [I 857; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 858
        !"test858"
        //let z0 = (7.8*(-y)-((-3.1)-(x*0.3)*((-6.7))+(-7.2))+(8.3+(7.7/(-y)*3.6-(-4.5)*(-8.3))+y*7.7+y/y))
        //printfn "%d" <| 858
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((7.8*(-y)-((-3.1)-(x*0.3)*((-6.7))+(-7.2))+(8.3+(7.7/(-y)*3.6-(-4.5)*(-8.3))+y*7.7+y/y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (7.8*(-q)-((-3.1)-(p*0.3)*((-6.7))+(-7.2))+(8.3+(7.7/(-q)*3.6-(-4.5)*(-8.3))+q*7.7+q/q))
            z2 <== (7.8*(-y)-((-3.1)-(x*0.3)*((-6.7))+(-7.2))+(8.3+(7.7/(-y)*3.6-(-4.5)*(-8.3))+y*7.7+y/y))
            wr [I 858; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 859
        !"test859"
        //let z0 = y
        //printfn "%d" <| 859
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 859; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 860
        !"test860"
        //let z0 = (-6.8)
        //printfn "%d" <| 860
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 861
        !"test861"
        //let z0 = y
        //printfn "%d" <| 861
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 861; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 862
        !"test862"
        //let z0 = (8.6/y+((y)-(3.7/(-3.8)-x)/((-3.0))*8.8-(x-(-8.6)/8.2))+((-5.2)+(-y)-(-4.8)+(-3.7))-(y/(-x)/y+(-y)*8.2)-(-x)*(5.7+x)/(8.3*y+(-5.4)*(-4.4)*(-x))*(y)+x+x*(-1.1)+(-3.0)-(-y)-8.6*((-x)*(-x)*(-7.5)-(-0.2))*((x*3.2*(-x))+((-2.6)/(-3.2))*y*x/(-3.2)*0.5))
        //printfn "%d" <| 862
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((8.6/y+((y)-(3.7/(-3.8)-x)/((-3.0))*8.8-(x-(-8.6)/8.2))+((-5.2)+(-y)-(-4.8)+(-3.7))-(y/(-x)/y+(-y)*8.2)-(-x)*(5.7+x)/(8.3*y+(-5.4)*(-4.4)*(-x))*(y)+x+x*(-1.1)+(-3.0)-(-y)-8.6*((-x)*(-x)*(-7.5)-(-0.2))*((x*3.2*(-x))+((-2.6)/(-3.2))*y*x/(-3.2)*0.5))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (8.6/q+((q)-(3.7/(-3.8)-p)/((-3.0))*8.8-(p-(-8.6)/8.2))+((-5.2)+(-q)-(-4.8)+(-3.7))-(q/(-p)/q+(-q)*8.2)-(-p)*(5.7+p)/(8.3*q+(-5.4)*(-4.4)*(-p))*(q)+p+p*(-1.1)+(-3.0)-(-q)-8.6*((-p)*(-p)*(-7.5)-(-0.2))*((p*3.2*(-p))+((-2.6)/(-3.2))*q*p/(-3.2)*0.5))
            z2 <== (8.6/y+((y)-(3.7/(-3.8)-x)/((-3.0))*8.8-(x-(-8.6)/8.2))+((-5.2)+(-y)-(-4.8)+(-3.7))-(y/(-x)/y+(-y)*8.2)-(-x)*(5.7+x)/(8.3*y+(-5.4)*(-4.4)*(-x))*(y)+x+x*(-1.1)+(-3.0)-(-y)-8.6*((-x)*(-x)*(-7.5)-(-0.2))*((x*3.2*(-x))+((-2.6)/(-3.2))*y*x/(-3.2)*0.5))
            wr [I 862; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 863
        !"test863"
        //let z0 = x
        //printfn "%d" <| 863
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 863; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 864
        !"test864"
        //let z0 = ((4.6+(x-(-5.2)*y/7.2-(-y))-1.5*((-y)/(-2.3)/(-y)*(-y)/(-2.1))+(-7.1))/(x))
        //printfn "%d" <| 864
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((4.6+(x-(-5.2)*y/7.2-(-y))-1.5*((-y)/(-2.3)/(-y)*(-y)/(-2.1))+(-7.1))/(x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((4.6+(p-(-5.2)*q/7.2-(-q))-1.5*((-q)/(-2.3)/(-q)*(-q)/(-2.1))+(-7.1))/(p))
            z2 <== ((4.6+(x-(-5.2)*y/7.2-(-y))-1.5*((-y)/(-2.3)/(-y)*(-y)/(-2.1))+(-7.1))/(x))
            wr [I 864; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 865
        !"test865"
        //let z0 = 6.1
        //printfn "%d" <| 865
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 866
        !"test866"
        //let z0 = (-7.4)
        //printfn "%d" <| 866
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 867
        !"test867"
        //let z0 = ((-8.3)*x/(3.1-y)*(-y)/6.2/y*((-x)-y-(-x)+(-2.2)+(-y))-(-2.8))
        //printfn "%d" <| 867
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-8.3)*x/(3.1-y)*(-y)/6.2/y*((-x)-y-(-x)+(-2.2)+(-y))-(-2.8))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-8.3)*p/(3.1-q)*(-q)/6.2/q*((-p)-q-(-p)+(-2.2)+(-q))-(-2.8))
            z2 <== ((-8.3)*x/(3.1-y)*(-y)/6.2/y*((-x)-y-(-x)+(-2.2)+(-y))-(-2.8))
            wr [I 867; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 868
        !"test868"
        //let z0 = ((-6.1))
        //printfn "%d" <| 868
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 869
        !"test869"
        //let z0 = x
        //printfn "%d" <| 869
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 869; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 870
        !"test870"
        //let z0 = 7.2
        //printfn "%d" <| 870
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 871
        !"test871"
        //let z0 = (-5.8)
        //printfn "%d" <| 871
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 872
        !"test872"
        //let z0 = ((-x)/((5.2*y*7.1)/x*(-4.7)))
        //printfn "%d" <| 872
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)/((5.2*y*7.1)/x*(-4.7)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)/((5.2*q*7.1)/p*(-4.7)))
            z2 <== ((-x)/((5.2*y*7.1)/x*(-4.7)))
            wr [I 872; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 873
        !"test873"
        //let z0 = y
        //printfn "%d" <| 873
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 873; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 874
        !"test874"
        //let z0 = (((2.3*(-0.1)/x-y-(-y))+y+(-x))+(y+(-x)/(y*(-y)+(-8.0)*y)+(y*(-4.7)*(-x)))-(-y)+(-2.5))
        //printfn "%d" <| 874
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((2.3*(-0.1)/x-y-(-y))+y+(-x))+(y+(-x)/(y*(-y)+(-8.0)*y)+(y*(-4.7)*(-x)))-(-y)+(-2.5))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((2.3*(-0.1)/p-q-(-q))+q+(-p))+(q+(-p)/(q*(-q)+(-8.0)*q)+(q*(-4.7)*(-p)))-(-q)+(-2.5))
            z2 <== (((2.3*(-0.1)/x-y-(-y))+y+(-x))+(y+(-x)/(y*(-y)+(-8.0)*y)+(y*(-4.7)*(-x)))-(-y)+(-2.5))
            wr [I 874; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 875
        !"test875"
        //let z0 = (-x)
        //printfn "%d" <| 875
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 875; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 876
        !"test876"
        //let z0 = (-y)+((-2.5)*(-y)*(-x)*x-((-y)*(-1.5))-x)-y*(-y)+(-y)+((-2.6)-x+(-x))
        //printfn "%d" <| 876
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)+((-2.5)*(-y)*(-x)*x-((-y)*(-1.5))-x)-y*(-y)+(-y)+((-2.6)-x+(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)+((-2.5)*(-q)*(-p)*p-((-q)*(-1.5))-p)-q*(-q)+(-q)+((-2.6)-p+(-p))
            z2 <== (-y)+((-2.5)*(-y)*(-x)*x-((-y)*(-1.5))-x)-y*(-y)+(-y)+((-2.6)-x+(-x))
            wr [I 876; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 877
        !"test877"
        //let z0 = 4.2
        //printfn "%d" <| 877
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 878
        !"test878"
        //let z0 = x/((-3.7)-(-6.8))
        //printfn "%d" <| 878
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x/((-3.7)-(-6.8))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p/((-3.7)-(-6.8))
            z2 <== x/((-3.7)-(-6.8))
            wr [I 878; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 879
        !"test879"
        //let z0 = (-x)
        //printfn "%d" <| 879
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 879; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 880
        !"test880"
        //let z0 = (-3.2)/(((-x)+(-x)/(-8.1)*x)/x)
        //printfn "%d" <| 880
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-3.2)/(((-x)+(-x)/(-8.1)*x)/x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-3.2)/(((-p)+(-p)/(-8.1)*p)/p)
            z2 <== (-3.2)/(((-x)+(-x)/(-8.1)*x)/x)
            wr [I 880; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 881
        !"test881"
        //let z0 = (-4.2)
        //printfn "%d" <| 881
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 882
        !"test882"
        //let z0 = (((-y)+((-x)*3.6)-((-y))-(-4.5)/8.1)/1.0/7.6+8.4-(y/(-y)+(-2.6)/4.1/7.5+(-x)))
        //printfn "%d" <| 882
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)+((-x)*3.6)-((-y))-(-4.5)/8.1)/1.0/7.6+8.4-(y/(-y)+(-2.6)/4.1/7.5+(-x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)+((-p)*3.6)-((-q))-(-4.5)/8.1)/1.0/7.6+8.4-(q/(-q)+(-2.6)/4.1/7.5+(-p)))
            z2 <== (((-y)+((-x)*3.6)-((-y))-(-4.5)/8.1)/1.0/7.6+8.4-(y/(-y)+(-2.6)/4.1/7.5+(-x)))
            wr [I 882; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 883
        !"test883"
        //let z0 = ((-y)-(((-x)*(-x)-(-7.7)+(-7.6)+(-8.6))-((-2.3))*((-y)*x)+(-y)-((-0.7)/(-4.6)*6.8)))
        //printfn "%d" <| 883
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)-(((-x)*(-x)-(-7.7)+(-7.6)+(-8.6))-((-2.3))*((-y)*x)+(-y)-((-0.7)/(-4.6)*6.8)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)-(((-p)*(-p)-(-7.7)+(-7.6)+(-8.6))-((-2.3))*((-q)*p)+(-q)-((-0.7)/(-4.6)*6.8)))
            z2 <== ((-y)-(((-x)*(-x)-(-7.7)+(-7.6)+(-8.6))-((-2.3))*((-y)*x)+(-y)-((-0.7)/(-4.6)*6.8)))
            wr [I 883; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 884
        !"test884"
        //let z0 = 4.8
        //printfn "%d" <| 884
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 885
        !"test885"
        //let z0 = (-3.2)
        //printfn "%d" <| 885
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 886
        !"test886"
        //let z0 = ((((-y)/(-1.2)/(-y)-(-7.1)+(-y))*((-1.7)-4.8+(-x)-(-y)))/((x)+x+(-x))*7.5)
        //printfn "%d" <| 886
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-y)/(-1.2)/(-y)-(-7.1)+(-y))*((-1.7)-4.8+(-x)-(-y)))/((x)+x+(-x))*7.5)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-q)/(-1.2)/(-q)-(-7.1)+(-q))*((-1.7)-4.8+(-p)-(-q)))/((p)+p+(-p))*7.5)
            z2 <== ((((-y)/(-1.2)/(-y)-(-7.1)+(-y))*((-1.7)-4.8+(-x)-(-y)))/((x)+x+(-x))*7.5)
            wr [I 886; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 887
        !"test887"
        //let z0 = 0.8
        //printfn "%d" <| 887
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 888
        !"test888"
        //let z0 = (-x)
        //printfn "%d" <| 888
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 888; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 889
        !"test889"
        //let z0 = (-7.7)/(-0.4)*((-0.1)-(-5.5)-(-8.7)*(-x)+(-x))*((-y))-(-y)-y+y
        //printfn "%d" <| 889
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-7.7)/(-0.4)*((-0.1)-(-5.5)-(-8.7)*(-x)+(-x))*((-y))-(-y)-y+y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-7.7)/(-0.4)*((-0.1)-(-5.5)-(-8.7)*(-p)+(-p))*((-q))-(-q)-q+q
            z2 <== (-7.7)/(-0.4)*((-0.1)-(-5.5)-(-8.7)*(-x)+(-x))*((-y))-(-y)-y+y
            wr [I 889; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 890
        !"test890"
        //let z0 = (y*(-2.3))
        //printfn "%d" <| 890
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y*(-2.3))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q*(-2.3))
            z2 <== (y*(-2.3))
            wr [I 890; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 891
        !"test891"
        //let z0 = (-4.1)
        //printfn "%d" <| 891
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 892
        !"test892"
        //let z0 = (4.6*(-3.4)*4.7+((-y)/(-3.1)-1.7))
        //printfn "%d" <| 892
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((4.6*(-3.4)*4.7+((-y)/(-3.1)-1.7))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (4.6*(-3.4)*4.7+((-q)/(-3.1)-1.7))
            z2 <== (4.6*(-3.4)*4.7+((-y)/(-3.1)-1.7))
            wr [I 892; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 893
        !"test893"
        //let z0 = 7.6
        //printfn "%d" <| 893
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 894
        !"test894"
        //let z0 = (-y)
        //printfn "%d" <| 894
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 894; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 895
        !"test895"
        //let z0 = (-1.8)
        //printfn "%d" <| 895
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 896
        !"test896"
        //let z0 = (y/(-x)-((7.1))/(-6.0)-x)
        //printfn "%d" <| 896
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y/(-x)-((7.1))/(-6.0)-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q/(-p)-((7.1))/(-6.0)-p)
            z2 <== (y/(-x)-((7.1))/(-6.0)-x)
            wr [I 896; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 897
        !"test897"
        //let z0 = ((((-6.4))*5.6*(2.4+0.1+(-y)/(-x)+3.1)))
        //printfn "%d" <| 897
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-6.4))*5.6*(2.4+0.1+(-y)/(-x)+3.1)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-6.4))*5.6*(2.4+0.1+(-q)/(-p)+3.1)))
            z2 <== ((((-6.4))*5.6*(2.4+0.1+(-y)/(-x)+3.1)))
            wr [I 897; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 898
        !"test898"
        //let z0 = ((7.0/y/(-4.4)-((-1.6)-y-(-4.1)))*(-y))
        //printfn "%d" <| 898
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((7.0/y/(-4.4)-((-1.6)-y-(-4.1)))*(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((7.0/q/(-4.4)-((-1.6)-q-(-4.1)))*(-q))
            z2 <== ((7.0/y/(-4.4)-((-1.6)-y-(-4.1)))*(-y))
            wr [I 898; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 899
        !"test899"
        //let z0 = y*1.2/(x*8.5)*(-x)*((y)*(-1.0))
        //printfn "%d" <| 899
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y*1.2/(x*8.5)*(-x)*((y)*(-1.0))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q*1.2/(p*8.5)*(-p)*((q)*(-1.0))
            z2 <== y*1.2/(x*8.5)*(-x)*((y)*(-1.0))
            wr [I 899; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 900
        !"test900"
        //let z0 = (-2.7)
        //printfn "%d" <| 900
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 901
        !"test901"
        //let z0 = x
        //printfn "%d" <| 901
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 901; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 902
        !"test902"
        //let z0 = (x+(((-6.3)+(-5.7)*x/(-3.6)+(-y))*(5.0)-1.8)+(((-2.8)/(-3.1)+(-y))-((-3.0)-(-1.5)/(-2.8)-(-6.4)))+(-y)*2.1)
        //printfn "%d" <| 902
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((x+(((-6.3)+(-5.7)*x/(-3.6)+(-y))*(5.0)-1.8)+(((-2.8)/(-3.1)+(-y))-((-3.0)-(-1.5)/(-2.8)-(-6.4)))+(-y)*2.1)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (p+(((-6.3)+(-5.7)*p/(-3.6)+(-q))*(5.0)-1.8)+(((-2.8)/(-3.1)+(-q))-((-3.0)-(-1.5)/(-2.8)-(-6.4)))+(-q)*2.1)
            z2 <== (x+(((-6.3)+(-5.7)*x/(-3.6)+(-y))*(5.0)-1.8)+(((-2.8)/(-3.1)+(-y))-((-3.0)-(-1.5)/(-2.8)-(-6.4)))+(-y)*2.1)
            wr [I 902; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 903
        !"test903"
        //let z0 = (((-4.6)-(-x)/(-4.8)-(x+y/x*(-2.7)))/y*((8.1+(-y)+8.8/(-x)+1.7)*(x-x)-x*(-3.6)-y*(-y)/(x*(-1.1)*x)))
        //printfn "%d" <| 903
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-4.6)-(-x)/(-4.8)-(x+y/x*(-2.7)))/y*((8.1+(-y)+8.8/(-x)+1.7)*(x-x)-x*(-3.6)-y*(-y)/(x*(-1.1)*x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-4.6)-(-p)/(-4.8)-(p+q/p*(-2.7)))/q*((8.1+(-q)+8.8/(-p)+1.7)*(p-p)-p*(-3.6)-q*(-q)/(p*(-1.1)*p)))
            z2 <== (((-4.6)-(-x)/(-4.8)-(x+y/x*(-2.7)))/y*((8.1+(-y)+8.8/(-x)+1.7)*(x-x)-x*(-3.6)-y*(-y)/(x*(-1.1)*x)))
            wr [I 903; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 904
        !"test904"
        //let z0 = (-6.1)
        //printfn "%d" <| 904
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 905
        !"test905"
        //let z0 = (-y)
        //printfn "%d" <| 905
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 905; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 906
        !"test906"
        //let z0 = ((4.3-x)*((-6.1)/((-8.5)*1.6+(-y))-(-y)))
        //printfn "%d" <| 906
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((4.3-x)*((-6.1)/((-8.5)*1.6+(-y))-(-y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((4.3-p)*((-6.1)/((-8.5)*1.6+(-q))-(-q)))
            z2 <== ((4.3-x)*((-6.1)/((-8.5)*1.6+(-y))-(-y)))
            wr [I 906; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 907
        !"test907"
        //let z0 = (3.5+((-4.4)+(-x)/(-x)-(-6.0))-(x*(2.5-y/(-6.7)+8.0)))
        //printfn "%d" <| 907
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((3.5+((-4.4)+(-x)/(-x)-(-6.0))-(x*(2.5-y/(-6.7)+8.0)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (3.5+((-4.4)+(-p)/(-p)-(-6.0))-(p*(2.5-q/(-6.7)+8.0)))
            z2 <== (3.5+((-4.4)+(-x)/(-x)-(-6.0))-(x*(2.5-y/(-6.7)+8.0)))
            wr [I 907; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 908
        !"test908"
        //let z0 = (-0.6)
        //printfn "%d" <| 908
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 909
        !"test909"
        //let z0 = (y)
        //printfn "%d" <| 909
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q)
            z2 <== (y)
            wr [I 909; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 910
        !"test910"
        //let z0 = (y+(0.4)*4.4+y-((-8.6)*(-x)*(-7.6)))
        //printfn "%d" <| 910
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+(0.4)*4.4+y-((-8.6)*(-x)*(-7.6)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+(0.4)*4.4+q-((-8.6)*(-p)*(-7.6)))
            z2 <== (y+(0.4)*4.4+y-((-8.6)*(-x)*(-7.6)))
            wr [I 910; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 911
        !"test911"
        //let z0 = y
        //printfn "%d" <| 911
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 911; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 912
        !"test912"
        //let z0 = ((-0.1)-x)
        //printfn "%d" <| 912
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.1)-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.1)-p)
            z2 <== ((-0.1)-x)
            wr [I 912; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 913
        !"test913"
        //let z0 = x
        //printfn "%d" <| 913
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 913; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 914
        !"test914"
        //let z0 = ((-x)*(7.0*8.3/(-x)*(-x))*((-y)+(-4.6)+(-1.8)*(-x)/(-3.8))-y+(0.3*y+x+(-6.4))*((-x)))
        //printfn "%d" <| 914
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x)*(7.0*8.3/(-x)*(-x))*((-y)+(-4.6)+(-1.8)*(-x)/(-3.8))-y+(0.3*y+x+(-6.4))*((-x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p)*(7.0*8.3/(-p)*(-p))*((-q)+(-4.6)+(-1.8)*(-p)/(-3.8))-q+(0.3*q+p+(-6.4))*((-p)))
            z2 <== ((-x)*(7.0*8.3/(-x)*(-x))*((-y)+(-4.6)+(-1.8)*(-x)/(-3.8))-y+(0.3*y+x+(-6.4))*((-x)))
            wr [I 914; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 915
        !"test915"
        //let z0 = (0.0*(x/((-3.6)+(-x)-(-x))+((-x)*y+(-x)*(-y))*6.5*2.5)/y*4.0+((-y)+8.4*(-x))-(-6.6)+((-8.0)/(-y)*(-5.6)+(-6.8)*(-x))+((-7.3)/x*y+1.8*x)-(x-(0.7+(-x))-((-x))-(2.1-(-6.3)*(-x)+(-0.1))-y))
        //printfn "%d" <| 915
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((0.0*(x/((-3.6)+(-x)-(-x))+((-x)*y+(-x)*(-y))*6.5*2.5)/y*4.0+((-y)+8.4*(-x))-(-6.6)+((-8.0)/(-y)*(-5.6)+(-6.8)*(-x))+((-7.3)/x*y+1.8*x)-(x-(0.7+(-x))-((-x))-(2.1-(-6.3)*(-x)+(-0.1))-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (0.0*(p/((-3.6)+(-p)-(-p))+((-p)*q+(-p)*(-q))*6.5*2.5)/q*4.0+((-q)+8.4*(-p))-(-6.6)+((-8.0)/(-q)*(-5.6)+(-6.8)*(-p))+((-7.3)/p*q+1.8*p)-(p-(0.7+(-p))-((-p))-(2.1-(-6.3)*(-p)+(-0.1))-q))
            z2 <== (0.0*(x/((-3.6)+(-x)-(-x))+((-x)*y+(-x)*(-y))*6.5*2.5)/y*4.0+((-y)+8.4*(-x))-(-6.6)+((-8.0)/(-y)*(-5.6)+(-6.8)*(-x))+((-7.3)/x*y+1.8*x)-(x-(0.7+(-x))-((-x))-(2.1-(-6.3)*(-x)+(-0.1))-y))
            wr [I 915; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 916
        !"test916"
        //let z0 = (((2.7/y-(-6.3)+x+3.1)/((-4.3)+x*x-2.4))*((-4.2)+(-1.1)+(-y)+x)/((y)+(-7.0)/y-x+2.2*(-3.1)*(-y)))
        //printfn "%d" <| 916
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((2.7/y-(-6.3)+x+3.1)/((-4.3)+x*x-2.4))*((-4.2)+(-1.1)+(-y)+x)/((y)+(-7.0)/y-x+2.2*(-3.1)*(-y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((2.7/q-(-6.3)+p+3.1)/((-4.3)+p*p-2.4))*((-4.2)+(-1.1)+(-q)+p)/((q)+(-7.0)/q-p+2.2*(-3.1)*(-q)))
            z2 <== (((2.7/y-(-6.3)+x+3.1)/((-4.3)+x*x-2.4))*((-4.2)+(-1.1)+(-y)+x)/((y)+(-7.0)/y-x+2.2*(-3.1)*(-y)))
            wr [I 916; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 917
        !"test917"
        //let z0 = y+x-y*(-x)*(-6.4)
        //printfn "%d" <| 917
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y+x-y*(-x)*(-6.4)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q+p-q*(-p)*(-6.4)
            z2 <== y+x-y*(-x)*(-6.4)
            wr [I 917; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 918
        !"test918"
        //let z0 = 4.1/(-7.0)/(-8.3)
        //printfn "%d" <| 918
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 919
        !"test919"
        //let z0 = (((-0.3))-(-5.7)/(-6.2)*1.2-y*(-1.2)/(-x)/(-2.2)-(x+y))
        //printfn "%d" <| 919
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-0.3))-(-5.7)/(-6.2)*1.2-y*(-1.2)/(-x)/(-2.2)-(x+y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-0.3))-(-5.7)/(-6.2)*1.2-q*(-1.2)/(-p)/(-2.2)-(p+q))
            z2 <== (((-0.3))-(-5.7)/(-6.2)*1.2-y*(-1.2)/(-x)/(-2.2)-(x+y))
            wr [I 919; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 920
        !"test920"
        //let z0 = ((-1.8)-(2.8/(-x)-(-2.3)*(-4.8))*0.7*5.5/(-7.3)-((-x)+(-x)/x+8.5))
        //printfn "%d" <| 920
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-1.8)-(2.8/(-x)-(-2.3)*(-4.8))*0.7*5.5/(-7.3)-((-x)+(-x)/x+8.5))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-1.8)-(2.8/(-p)-(-2.3)*(-4.8))*0.7*5.5/(-7.3)-((-p)+(-p)/p+8.5))
            z2 <== ((-1.8)-(2.8/(-x)-(-2.3)*(-4.8))*0.7*5.5/(-7.3)-((-x)+(-x)/x+8.5))
            wr [I 920; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 921
        !"test921"
        //let z0 = ((x)-(-x))
        //printfn "%d" <| 921
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x)-(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p)-(-p))
            z2 <== ((x)-(-x))
            wr [I 921; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 922
        !"test922"
        //let z0 = (-y)
        //printfn "%d" <| 922
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 922; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 923
        !"test923"
        //let z0 = ((x*1.8/(y-6.7+(-6.8))+(-x)+(-2.6))-(((-x)/(-2.5)/0.0-x)+((-x)*2.6*x/(-y))))
        //printfn "%d" <| 923
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x*1.8/(y-6.7+(-6.8))+(-x)+(-2.6))-(((-x)/(-2.5)/0.0-x)+((-x)*2.6*x/(-y))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p*1.8/(q-6.7+(-6.8))+(-p)+(-2.6))-(((-p)/(-2.5)/0.0-p)+((-p)*2.6*p/(-q))))
            z2 <== ((x*1.8/(y-6.7+(-6.8))+(-x)+(-2.6))-(((-x)/(-2.5)/0.0-x)+((-x)*2.6*x/(-y))))
            wr [I 923; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 924
        !"test924"
        //let z0 = (-x)
        //printfn "%d" <| 924
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 924; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 925
        !"test925"
        //let z0 = (-8.4)
        //printfn "%d" <| 925
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 926
        !"test926"
        //let z0 = (-1.5)
        //printfn "%d" <| 926
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 927
        !"test927"
        //let z0 = y
        //printfn "%d" <| 927
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 927; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 928
        !"test928"
        //let z0 = y
        //printfn "%d" <| 928
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 928; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 929
        !"test929"
        //let z0 = ((x+(-8.4)*(5.2+y-1.1)+(8.1/7.4)/0.2)-x-5.4-0.1/((-8.4)))
        //printfn "%d" <| 929
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x+(-8.4)*(5.2+y-1.1)+(8.1/7.4)/0.2)-x-5.4-0.1/((-8.4)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p+(-8.4)*(5.2+q-1.1)+(8.1/7.4)/0.2)-p-5.4-0.1/((-8.4)))
            z2 <== ((x+(-8.4)*(5.2+y-1.1)+(8.1/7.4)/0.2)-x-5.4-0.1/((-8.4)))
            wr [I 929; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 930
        !"test930"
        //let z0 = 7.5
        //printfn "%d" <| 930
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 931
        !"test931"
        //let z0 = (3.6*(-5.8)+(((-6.2)+3.2-3.3/(-2.2)-(-y))*(-x)-6.3/((-x)-x)+(-5.5))+(1.6-y/(-6.6)*x/y*x/(5.5-(-5.5)-x)))
        //printfn "%d" <| 931
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((3.6*(-5.8)+(((-6.2)+3.2-3.3/(-2.2)-(-y))*(-x)-6.3/((-x)-x)+(-5.5))+(1.6-y/(-6.6)*x/y*x/(5.5-(-5.5)-x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (3.6*(-5.8)+(((-6.2)+3.2-3.3/(-2.2)-(-q))*(-p)-6.3/((-p)-p)+(-5.5))+(1.6-q/(-6.6)*p/q*p/(5.5-(-5.5)-p)))
            z2 <== (3.6*(-5.8)+(((-6.2)+3.2-3.3/(-2.2)-(-y))*(-x)-6.3/((-x)-x)+(-5.5))+(1.6-y/(-6.6)*x/y*x/(5.5-(-5.5)-x)))
            wr [I 931; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 932
        !"test932"
        //let z0 = y
        //printfn "%d" <| 932
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 932; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 933
        !"test933"
        //let z0 = (-2.1)
        //printfn "%d" <| 933
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 934
        !"test934"
        //let z0 = (((-x)-(-x)*(3.4+(-x))*((-x)+(-y)+(-2.5)-x)-(x))*(2.8)*((-y)/x-(-x)-(-y))/(2.4-(-0.2))+(-y)*(-7.3)/y/(-x)/(-5.2))
        //printfn "%d" <| 934
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-x)-(-x)*(3.4+(-x))*((-x)+(-y)+(-2.5)-x)-(x))*(2.8)*((-y)/x-(-x)-(-y))/(2.4-(-0.2))+(-y)*(-7.3)/y/(-x)/(-5.2))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-p)-(-p)*(3.4+(-p))*((-p)+(-q)+(-2.5)-p)-(p))*(2.8)*((-q)/p-(-p)-(-q))/(2.4-(-0.2))+(-q)*(-7.3)/q/(-p)/(-5.2))
            z2 <== (((-x)-(-x)*(3.4+(-x))*((-x)+(-y)+(-2.5)-x)-(x))*(2.8)*((-y)/x-(-x)-(-y))/(2.4-(-0.2))+(-y)*(-7.3)/y/(-x)/(-5.2))
            wr [I 934; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 935
        !"test935"
        //let z0 = 3.6
        //printfn "%d" <| 935
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 936
        !"test936"
        //let z0 = (-x)
        //printfn "%d" <| 936
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 936; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 937
        !"test937"
        //let z0 = y
        //printfn "%d" <| 937
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 937; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 938
        !"test938"
        //let z0 = y
        //printfn "%d" <| 938
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 938; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 939
        !"test939"
        //let z0 = (3.1-(((-x)+4.0-(-x)*x*x)-3.3+((-0.3)*(-y)-y+(-x)/y)/((-x)+y))*3.2-(4.2*(-x)/x/y)*(-7.1)-2.7*(-y))
        //printfn "%d" <| 939
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((3.1-(((-x)+4.0-(-x)*x*x)-3.3+((-0.3)*(-y)-y+(-x)/y)/((-x)+y))*3.2-(4.2*(-x)/x/y)*(-7.1)-2.7*(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (3.1-(((-p)+4.0-(-p)*p*p)-3.3+((-0.3)*(-q)-q+(-p)/q)/((-p)+q))*3.2-(4.2*(-p)/p/q)*(-7.1)-2.7*(-q))
            z2 <== (3.1-(((-x)+4.0-(-x)*x*x)-3.3+((-0.3)*(-y)-y+(-x)/y)/((-x)+y))*3.2-(4.2*(-x)/x/y)*(-7.1)-2.7*(-y))
            wr [I 939; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 940
        !"test940"
        //let z0 = ((y+(-5.1)/(-1.7)-5.2)-7.7+(-y)/(-7.3)/((-3.0)*(-0.4)))*((x-y-7.3-4.3-8.2)-((-1.6))-x-8.5-5.0-(-2.2)/(-4.1)/(-x))-4.3-x+((-4.2)-5.3*x/(-y)*8.1*y+(x+4.8/x)-6.0-(-0.6)-(-7.6))
        //printfn "%d" <| 940
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y+(-5.1)/(-1.7)-5.2)-7.7+(-y)/(-7.3)/((-3.0)*(-0.4)))*((x-y-7.3-4.3-8.2)-((-1.6))-x-8.5-5.0-(-2.2)/(-4.1)/(-x))-4.3-x+((-4.2)-5.3*x/(-y)*8.1*y+(x+4.8/x)-6.0-(-0.6)-(-7.6))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q+(-5.1)/(-1.7)-5.2)-7.7+(-q)/(-7.3)/((-3.0)*(-0.4)))*((p-q-7.3-4.3-8.2)-((-1.6))-p-8.5-5.0-(-2.2)/(-4.1)/(-p))-4.3-p+((-4.2)-5.3*p/(-q)*8.1*q+(p+4.8/p)-6.0-(-0.6)-(-7.6))
            z2 <== ((y+(-5.1)/(-1.7)-5.2)-7.7+(-y)/(-7.3)/((-3.0)*(-0.4)))*((x-y-7.3-4.3-8.2)-((-1.6))-x-8.5-5.0-(-2.2)/(-4.1)/(-x))-4.3-x+((-4.2)-5.3*x/(-y)*8.1*y+(x+4.8/x)-6.0-(-0.6)-(-7.6))
            wr [I 940; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 941
        !"test941"
        //let z0 = x
        //printfn "%d" <| 941
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 941; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 942
        !"test942"
        //let z0 = (-6.8)
        //printfn "%d" <| 942
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 943
        !"test943"
        //let z0 = ((-4.2)*(((-y)*(-y)/5.6-y*1.1)*(-8.8)/6.1+x/(-3.4)))
        //printfn "%d" <| 943
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-4.2)*(((-y)*(-y)/5.6-y*1.1)*(-8.8)/6.1+x/(-3.4)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-4.2)*(((-q)*(-q)/5.6-q*1.1)*(-8.8)/6.1+p/(-3.4)))
            z2 <== ((-4.2)*(((-y)*(-y)/5.6-y*1.1)*(-8.8)/6.1+x/(-3.4)))
            wr [I 943; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 944
        !"test944"
        //let z0 = ((2.5-x+5.8-(-4.4)))
        //printfn "%d" <| 944
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((2.5-x+5.8-(-4.4)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((2.5-p+5.8-(-4.4)))
            z2 <== ((2.5-x+5.8-(-4.4)))
            wr [I 944; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 945
        !"test945"
        //let z0 = ((-1.0)-(-x)*(-y))
        //printfn "%d" <| 945
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-1.0)-(-x)*(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-1.0)-(-p)*(-q))
            z2 <== ((-1.0)-(-x)*(-y))
            wr [I 945; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 946
        !"test946"
        //let z0 = (-y)
        //printfn "%d" <| 946
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 946; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 947
        !"test947"
        //let z0 = 5.3
        //printfn "%d" <| 947
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 948
        !"test948"
        //let z0 = x
        //printfn "%d" <| 948
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 948; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 949
        !"test949"
        //let z0 = x
        //printfn "%d" <| 949
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 949; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 950
        !"test950"
        //let z0 = x
        //printfn "%d" <| 950
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 950; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 951
        !"test951"
        //let z0 = (1.8*((-2.5)*(-y))+x)-(-x)
        //printfn "%d" <| 951
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((1.8*((-2.5)*(-y))+x)-(-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (1.8*((-2.5)*(-q))+p)-(-p)
            z2 <== (1.8*((-2.5)*(-y))+x)-(-x)
            wr [I 951; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 952
        !"test952"
        //let z0 = (-y)
        //printfn "%d" <| 952
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 952; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 953
        !"test953"
        //let z0 = (-y)/((-y)/3.7*y-((-y)))
        //printfn "%d" <| 953
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)/((-y)/3.7*y-((-y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)/((-q)/3.7*q-((-q)))
            z2 <== (-y)/((-y)/3.7*y-((-y)))
            wr [I 953; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 954
        !"test954"
        //let z0 = ((-6.6)+((x-1.6)))
        //printfn "%d" <| 954
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-6.6)+((x-1.6)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-6.6)+((p-1.6)))
            z2 <== ((-6.6)+((x-1.6)))
            wr [I 954; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 955
        !"test955"
        //let z0 = (-x)
        //printfn "%d" <| 955
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 955; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 956
        !"test956"
        //let z0 = ((-8.0)/((x)*(-1.6))-x)
        //printfn "%d" <| 956
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-8.0)/((x)*(-1.6))-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-8.0)/((p)*(-1.6))-p)
            z2 <== ((-8.0)/((x)*(-1.6))-x)
            wr [I 956; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 957
        !"test957"
        //let z0 = y
        //printfn "%d" <| 957
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 957; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 958
        !"test958"
        //let z0 = x
        //printfn "%d" <| 958
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 958; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 959
        !"test959"
        //let z0 = ((-y)*(-7.6))
        //printfn "%d" <| 959
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)*(-7.6))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)*(-7.6))
            z2 <== ((-y)*(-7.6))
            wr [I 959; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 960
        !"test960"
        //let z0 = (-4.5)
        //printfn "%d" <| 960
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 961
        !"test961"
        //let z0 = ((-y)*(-x))
        //printfn "%d" <| 961
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)*(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)*(-p))
            z2 <== ((-y)*(-x))
            wr [I 961; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 962
        !"test962"
        //let z0 = (((y+y*x)*y/((-8.7))+(-0.1)-(-7.2))*8.7*(-6.6)/(-y)*x/(-x))
        //printfn "%d" <| 962
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((y+y*x)*y/((-8.7))+(-0.1)-(-7.2))*8.7*(-6.6)/(-y)*x/(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((q+q*p)*q/((-8.7))+(-0.1)-(-7.2))*8.7*(-6.6)/(-q)*p/(-p))
            z2 <== (((y+y*x)*y/((-8.7))+(-0.1)-(-7.2))*8.7*(-6.6)/(-y)*x/(-x))
            wr [I 962; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 963
        !"test963"
        //let z0 = y
        //printfn "%d" <| 963
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 963; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 964
        !"test964"
        //let z0 = ((-y)*y)
        //printfn "%d" <| 964
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)*y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)*q)
            z2 <== ((-y)*y)
            wr [I 964; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 965
        !"test965"
        //let z0 = (6.3/y+(-x)/(1.4*(-5.2)-y-y-y+(y*(-y)/y+(-7.8)-5.5)*((-0.6)-x*(-6.4))))
        //printfn "%d" <| 965
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((6.3/y+(-x)/(1.4*(-5.2)-y-y-y+(y*(-y)/y+(-7.8)-5.5)*((-0.6)-x*(-6.4))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (6.3/q+(-p)/(1.4*(-5.2)-q-q-q+(q*(-q)/q+(-7.8)-5.5)*((-0.6)-p*(-6.4))))
            z2 <== (6.3/y+(-x)/(1.4*(-5.2)-y-y-y+(y*(-y)/y+(-7.8)-5.5)*((-0.6)-x*(-6.4))))
            wr [I 965; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 966
        !"test966"
        //let z0 = 0.7
        //printfn "%d" <| 966
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 967
        !"test967"
        //let z0 = (-y)
        //printfn "%d" <| 967
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 967; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 968
        !"test968"
        //let z0 = x
        //printfn "%d" <| 968
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 968; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 969
        !"test969"
        //let z0 = (-7.3)+7.0*y*(-0.3)
        //printfn "%d" <| 969
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-7.3)+7.0*y*(-0.3)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-7.3)+7.0*q*(-0.3)
            z2 <== (-7.3)+7.0*y*(-0.3)
            wr [I 969; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 970
        !"test970"
        //let z0 = y
        //printfn "%d" <| 970
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (y).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== q
            z2 <== y
            wr [I 970; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 971
        !"test971"
        //let z0 = ((((-3.4)*(-x)+(-y))*(x))*(6.7-(0.0/(-6.6)-(-y)-(-y)))-((x)+(0.3)/(-6.0))/((-y)-(x/(-x)/(-8.5)*(-3.3))))
        //printfn "%d" <| 971
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-3.4)*(-x)+(-y))*(x))*(6.7-(0.0/(-6.6)-(-y)-(-y)))-((x)+(0.3)/(-6.0))/((-y)-(x/(-x)/(-8.5)*(-3.3))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-3.4)*(-p)+(-q))*(p))*(6.7-(0.0/(-6.6)-(-q)-(-q)))-((p)+(0.3)/(-6.0))/((-q)-(p/(-p)/(-8.5)*(-3.3))))
            z2 <== ((((-3.4)*(-x)+(-y))*(x))*(6.7-(0.0/(-6.6)-(-y)-(-y)))-((x)+(0.3)/(-6.0))/((-y)-(x/(-x)/(-8.5)*(-3.3))))
            wr [I 971; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 972
        !"test972"
        //let z0 = (8.4+(-5.4)-6.8)
        //printfn "%d" <| 972
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 973
        !"test973"
        //let z0 = (-7.2)
        //printfn "%d" <| 973
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 974
        !"test974"
        //let z0 = ((y+(-y)*7.7+x)/x-2.1-(-8.7)/y+((-5.6)+(-y)-y)/((-x)*8.5+1.5+6.7)*(-x))
        //printfn "%d" <| 974
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y+(-y)*7.7+x)/x-2.1-(-8.7)/y+((-5.6)+(-y)-y)/((-x)*8.5+1.5+6.7)*(-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q+(-q)*7.7+p)/p-2.1-(-8.7)/q+((-5.6)+(-q)-q)/((-p)*8.5+1.5+6.7)*(-p))
            z2 <== ((y+(-y)*7.7+x)/x-2.1-(-8.7)/y+((-5.6)+(-y)-y)/((-x)*8.5+1.5+6.7)*(-x))
            wr [I 974; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 975
        !"test975"
        //let z0 = (y+(5.7/(-7.2)/8.1/(-6.7)-x))
        //printfn "%d" <| 975
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y+(5.7/(-7.2)/8.1/(-6.7)-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q+(5.7/(-7.2)/8.1/(-6.7)-p))
            z2 <== (y+(5.7/(-7.2)/8.1/(-6.7)-x))
            wr [I 975; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 976
        !"test976"
        //let z0 = ((y*x-(-4.2))+((-x)*x)*(-8.5))+8.4
        //printfn "%d" <| 976
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((y*x-(-4.2))+((-x)*x)*(-8.5))+8.4).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((q*p-(-4.2))+((-p)*p)*(-8.5))+8.4
            z2 <== ((y*x-(-4.2))+((-x)*x)*(-8.5))+8.4
            wr [I 976; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 977
        !"test977"
        //let z0 = ((-7.0)+(((-5.2)*(-7.5)*(-3.0)))-((-x)/(-7.4)*(-7.6)+(-x)))
        //printfn "%d" <| 977
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-7.0)+(((-5.2)*(-7.5)*(-3.0)))-((-x)/(-7.4)*(-7.6)+(-x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-7.0)+(((-5.2)*(-7.5)*(-3.0)))-((-p)/(-7.4)*(-7.6)+(-p)))
            z2 <== ((-7.0)+(((-5.2)*(-7.5)*(-3.0)))-((-x)/(-7.4)*(-7.6)+(-x)))
            wr [I 977; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 978
        !"test978"
        //let z0 = (-2.4)
        //printfn "%d" <| 978
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 979
        !"test979"
        //let z0 = (-x)
        //printfn "%d" <| 979
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-p)
            z2 <== (-x)
            wr [I 979; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 980
        !"test980"
        //let z0 = ((-x))
        //printfn "%d" <| 980
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-x))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-p))
            z2 <== ((-x))
            wr [I 980; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 981
        !"test981"
        //let z0 = x
        //printfn "%d" <| 981
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 981; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 982
        !"test982"
        //let z0 = (1.4)
        //printfn "%d" <| 982
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 983
        !"test983"
        //let z0 = ((-y)*((8.1+(-8.8)-2.7*5.0)-(7.0+2.3/(-2.8)-6.1)+x)/(6.5+(y-(-4.6)/(-x)))+(((-x)*(-y)/(-y)-1.6)/((-6.1)/(-y)/(-x))))
        //printfn "%d" <| 983
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-y)*((8.1+(-8.8)-2.7*5.0)-(7.0+2.3/(-2.8)-6.1)+x)/(6.5+(y-(-4.6)/(-x)))+(((-x)*(-y)/(-y)-1.6)/((-6.1)/(-y)/(-x))))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-q)*((8.1+(-8.8)-2.7*5.0)-(7.0+2.3/(-2.8)-6.1)+p)/(6.5+(q-(-4.6)/(-p)))+(((-p)*(-q)/(-q)-1.6)/((-6.1)/(-q)/(-p))))
            z2 <== ((-y)*((8.1+(-8.8)-2.7*5.0)-(7.0+2.3/(-2.8)-6.1)+x)/(6.5+(y-(-4.6)/(-x)))+(((-x)*(-y)/(-y)-1.6)/((-6.1)/(-y)/(-x))))
            wr [I 983; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 984
        !"test984"
        //let z0 = ((-0.2)*(-5.2)/(-7.8)-((-3.4)+7.0-(-5.0)-0.8)/(-x)*7.1)
        //printfn "%d" <| 984
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.2)*(-5.2)/(-7.8)-((-3.4)+7.0-(-5.0)-0.8)/(-x)*7.1)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.2)*(-5.2)/(-7.8)-((-3.4)+7.0-(-5.0)-0.8)/(-p)*7.1)
            z2 <== ((-0.2)*(-5.2)/(-7.8)-((-3.4)+7.0-(-5.0)-0.8)/(-x)*7.1)
            wr [I 984; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 985
        !"test985"
        //let z0 = (-y)
        //printfn "%d" <| 985
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 985; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 986
        !"test986"
        //let z0 = (-4.6)
        //printfn "%d" <| 986
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 987
        !"test987"
        //let z0 = (-y)
        //printfn "%d" <| 987
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((-y)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (-q)
            z2 <== (-y)
            wr [I 987; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 988
        !"test988"
        //let z0 = (y-(-6.0)/(-x)*(7.3/(-y)-(-x))+4.1+(x)/(-0.1)+x)
        //printfn "%d" <| 988
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((y-(-6.0)/(-x)*(7.3/(-y)-(-x))+4.1+(x)/(-0.1)+x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (q-(-6.0)/(-p)*(7.3/(-q)-(-p))+4.1+(p)/(-0.1)+p)
            z2 <== (y-(-6.0)/(-x)*(7.3/(-y)-(-x))+4.1+(x)/(-0.1)+x)
            wr [I 988; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 989
        !"test989"
        //let z0 = ((x)-(8.2*3.5/(5.7+(-0.3)-x)))
        //printfn "%d" <| 989
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((x)-(8.2*3.5/(5.7+(-0.3)-x)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((p)-(8.2*3.5/(5.7+(-0.3)-p)))
            z2 <== ((x)-(8.2*3.5/(5.7+(-0.3)-x)))
            wr [I 989; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 990
        !"test990"
        //let z0 = (-5.1)
        //printfn "%d" <| 990
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 991
        !"test991"
        //let z0 = ((((-8.3)+0.6)-((-y)+(-7.0)/6.5+3.1*1.5))*1.7-(-x)-(-5.4)+(-x)+(-y))
        //printfn "%d" <| 991
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((((-8.3)+0.6)-((-y)+(-7.0)/6.5+3.1*1.5))*1.7-(-x)-(-5.4)+(-x)+(-y))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((((-8.3)+0.6)-((-q)+(-7.0)/6.5+3.1*1.5))*1.7-(-p)-(-5.4)+(-p)+(-q))
            z2 <== ((((-8.3)+0.6)-((-y)+(-7.0)/6.5+3.1*1.5))*1.7-(-x)-(-5.4)+(-x)+(-y))
            wr [I 991; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 992
        !"test992"
        //let z0 = 2.2
        //printfn "%d" <| 992
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 993
        !"test993"
        //let z0 = ((-0.8)-(5.8/x+5.7*3.5/(-3.4)*(-7.3)*(1.6*(-y)-x*6.6))/x)
        //printfn "%d" <| 993
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-0.8)-(5.8/x+5.7*3.5/(-3.4)*(-7.3)*(1.6*(-y)-x*6.6))/x)).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-0.8)-(5.8/p+5.7*3.5/(-3.4)*(-7.3)*(1.6*(-q)-p*6.6))/p)
            z2 <== ((-0.8)-(5.8/x+5.7*3.5/(-3.4)*(-7.3)*(1.6*(-y)-x*6.6))/x)
            wr [I 993; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 994
        !"test994"
        //let z0 = ((-2.7)*0.2*y/(-4.3))
        //printfn "%d" <| 994
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-2.7)*0.2*y/(-4.3))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-2.7)*0.2*q/(-4.3))
            z2 <== ((-2.7)*0.2*y/(-4.3))
            wr [I 994; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 995
        !"test995"
        //let z0 = x
        //printfn "%d" <| 995
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== p
            z2 <== x
            wr [I 995; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 996
        !"test996"
        //let z0 = ((-3.7)-y/(-2.4)+(-y))-3.3*x
        //printfn "%d" <| 996
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-3.7)-y/(-2.4)+(-y))-3.3*x).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-3.7)-q/(-2.4)+(-q))-3.3*p
            z2 <== ((-3.7)-y/(-2.4)+(-y))-3.3*x
            wr [I 996; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 997
        !"test997"
        //let z0 = ((-2.0)/(y-(-7.2)*3.0)+(1.2+x+x)+(-4.8)+(-y)/(-y)*x+(-y)*(-x)-(-0.3)+x-(((-4.2)-(-3.8)*5.5)-x+((-4.2)-x+0.4)*(-0.4)-(-y)+(-4.8)+7.2-(-y)/(y*(-1.5)+(-y)*(-6.6)*7.4)))
        //printfn "%d" <| 997
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = (((-2.0)/(y-(-7.2)*3.0)+(1.2+x+x)+(-4.8)+(-y)/(-y)*x+(-y)*(-x)-(-0.3)+x-(((-4.2)-(-3.8)*5.5)-x+((-4.2)-x+0.4)*(-0.4)-(-y)+(-4.8)+7.2-(-y)/(y*(-1.5)+(-y)*(-6.6)*7.4)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== ((-2.0)/(q-(-7.2)*3.0)+(1.2+p+p)+(-4.8)+(-q)/(-q)*p+(-q)*(-p)-(-0.3)+p-(((-4.2)-(-3.8)*5.5)-p+((-4.2)-p+0.4)*(-0.4)-(-q)+(-4.8)+7.2-(-q)/(q*(-1.5)+(-q)*(-6.6)*7.4)))
            z2 <== ((-2.0)/(y-(-7.2)*3.0)+(1.2+x+x)+(-4.8)+(-y)/(-y)*x+(-y)*(-x)-(-0.3)+x-(((-4.2)-(-3.8)*5.5)-x+((-4.2)-x+0.4)*(-0.4)-(-y)+(-4.8)+7.2-(-y)/(y*(-1.5)+(-y)*(-6.6)*7.4)))
            wr [I 997; z1; z2; asm.abs(z1-z2);]
        //printfn "%d" 998
        !"test998"
        //let z0 = 8.6
        //printfn "%d" <| 998
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 999
        !"test999"
        //let z0 = 1.6
        //printfn "%d" <| 999
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        //printfn "%d" 1000
        !"test1000"
        //let z0 = (((-y)/((-7.0)*(-3.4)*y)))
        //printfn "%d" <| 1000
        //printfn "original:"
        //printfn "%s" <| z0.Expr.ToString()
        //printfn "simp:"
        //printfn "%s" <| z0.Expr.simp.ToString()
        let s = ((((-y)/((-7.0)*(-3.4)*y)))).Expr.eval pr
        if (not <| s.ToString().Contains("NaN")) && (not <| s.ToString().Contains("∞")) then
            z1 <== (((-q)/((-7.0)*(-3.4)*q)))
            z2 <== (((-y)/((-7.0)*(-3.4)*y)))
            wr [I 1000; z1; z2; asm.abs(z1-z2);]
