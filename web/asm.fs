namespace docWriter

open System.IO

type asm =
    static member uj = Var_e "j"  
    static member pi = Var_e "\\pi"
    static member abs(x:num0) = Abs x
    static member pow(x:num0,y:num0) =Pow(x,y)
    static member pow(x:num0,y:int) = Pow(x, Int_e y)
    static member pow(x:num0,y:double) = Pow(x, Dbl_e y)
    static member pow(x:int,y:num0) = Pow(Int_e x, y)
    static member pow(x:double,y:num0) = Pow(Dbl_e x, y)
    static member exp(x:num0) = Exp x
    static member sin(x:num0) = Sin x
    static member cos(x:num0) = Cos x
    static member tan(x:num0) = Tan x
    static member asin(x:num0) = Asin x
    static member acos(x:num0) = Acos x
    static member atan(x:num0) = Atan x
    static member log(x:num0) = Log x
    static member log10(x:num0) = Log10 x
    static member sqrt(x:num0) = Sqrt x
    static member sum(k:num0,s:num0,n:num0,t:num0) = Var_e ("\\displaystyle\\sum_{"+k.str LaTeX + "="+s.str LaTeX+"}^{"+n.str LaTeX+"}"+t.str LaTeX)
    static member sum(k:num0,s:int,n:num0,t:num0) = Var_e ("\\displaystyle\\sum_{"+k.str LaTeX+"="+(Int_e s).str LaTeX+"}^{"+n.str LaTeX+"}"+t.str LaTeX)
    static member sum(k:num0,s:num0,n:int,t:num0) = Var_e ("\\displaystyle\\sum_{"+k.str LaTeX+"="+s.str LaTeX+"}^{"+(Int_e n).str LaTeX+"}"+t.str LaTeX)
    static member sum(k:num0,s:int,n:int,t:num0) = Var_e ("\\displaystyle\\sum_{"+k.str LaTeX+"="+(Int_e s).str LaTeX+"}^{"+(Int_e n).str LaTeX+"}"+t.str LaTeX)
    static member sum(n:num0,k:num0,t:num0) = Var_e ("\\displaystyle\\sum_{"+k.str LaTeX+"}^{"+n.str LaTeX+"}"+t.str LaTeX)
    static member integral(x:num0,y:num0,s:num0,t:num0) = Var_e ("\\displaystyle\\int_{"+x.str LaTeX+"}^{"+y.str LaTeX+"}"+t.str LaTeX+"d"+s.str LaTeX)
    static member integral(x:int,y:num0,s:num0,t:num0) = Var_e ("\\displaystyle\\int_{"+(Int_e x).str LaTeX+"}^{"+y.str LaTeX+"}"+t.str LaTeX+"d"+s.str LaTeX)
    static member integral(x:num0,y:int,s:num0,t:num0) = Var_e ("\\displaystyle\\int_{"+x.str LaTeX+"}^{"+(Int_e y).str LaTeX+"}"+t.str LaTeX+"d"+s.str LaTeX)
    static member integral(x:int,y:int,s:num0,t:num0) = Var_e ("\\displaystyle\\int_{"+(Int_e x).str LaTeX+"}^{"+(Int_e y).str LaTeX+"}"+t.str LaTeX+"d"+s.str LaTeX)
    static member integral(t:num0,s:num0) = Var_e ("\\int_"+t.str LaTeX+"d"+s.str LaTeX)
    static member diff(x:num0,y:num0) = Var_e ("\\dfrac{d"+x.str LaTeX+"}"+"{d"+y.str LaTeX+"}" )
    static member pdiff(x:num0,y:num0) = Var_e ("\\dfrac{\\partial"+x.str LaTeX+"}"+"{\\partial"+y.str LaTeX+"}" )
    static member vdots =Var_e "\\vdots"

    static member cdots =Var_e "\\cdots"
    static member brace(x:num0) = Var_e ("\\left["+x.str LaTeX+"\\right]")
    static member cbrace(x:num0) = Var_e ("\\left\\{"+x.str LaTeX+"\\right\\}")
