namespace docWriter

open System.IO

type CodeLanguage = |LaTeX |JavaScript |Text

type num0 =
    |Int_e of int
    |Dbl_e of double
    |Var_e of string
    |Vec_e of num0
    |Mat_e of num0
    |Num_e of num0
    |Add of num0*num0
    |Sub of num0*num0
    |Mul of num0*num0
    |Div of num0*num0
    |Pow of num0*num0
    |Sin of num0
    |Cos of num0
    |Tan of num0
    |Asin of num0
    |Acos of num0
    |Atan of num0
    |Atan2 of num0*num0
    |Exp of num0
    |Log of num0
    |Log10 of num0
    |Sqrt of num0
    |Abs of num0
    |Equ of num0*num0
    |NotEqu of num0*num0
    |NearEqu of num0*num0
    |Greater of num0*num0
    |Smaller of num0*num0
    |GreaterEqu of num0*num0
    |SmallerEqu of num0*num0
    
    static member (+) (x:num0,y:num0) = Add(x,y)
    static member (+) (x:num0,y:int) = Add(x,Int_e y)
    static member (+) (x:num0,y:double) = Add(x,Dbl_e y)
    static member (+) (x:int,y:num0) = Add(Int_e x,y)    
    static member (+) (x:double,y:num0) = Add(Dbl_e x,y)
    
    static member (-) (x:num0,y:num0) = Sub(x,y)
    static member (-) (x:num0,y:int) = Sub(x,Int_e y)
    static member (-) (x:num0,y:double) = Sub(x,Dbl_e y)
    static member (-) (x:int,y:num0) = Sub(Int_e x,y)
    static member (-) (x:double,y:num0) = Sub(Dbl_e x, y)
    
    static member ( * ) (x:num0,y:num0) = Mul(x,y)
    static member ( * ) (x:num0,y:int) = Mul(Int_e y,x)
    static member ( * ) (x:num0,y:double) = Mul(Dbl_e y,x)
    static member ( * ) (x:int,y:num0) = Mul(Int_e x,y)
    static member ( * ) (x:double,y:num0) =  Mul(Dbl_e x,y)
    
    static member (/) (x:num0,y:num0) = Div(x,y)
    static member (/) (x:num0,y:int) = Div(x,Int_e y)
    static member (/) (x:num0,y:double) = Div(x,Dbl_e y)
    static member (/) (x:int,y:num0) = Div(Int_e x, y)
    static member (/) (x:double,y:num0) = Div(Dbl_e x,y)
    
    static member (<==) (x:num0,y:num0) = Equ(x,y)
    static member (<==) (x:num0,y:int) = Equ(x,Int_e y)
    static member (<==) (x:num0,y:double) = Equ(x,Dbl_e y)
    static member (<==) (x:int,y:num0) = Equ(Int_e x, y)
    static member (<==) (x:double,y:num0) = Equ(Dbl_e x,y)
    
    static member (<==.) (x:num0,y:num0) = NearEqu(x,y)
    static member (<==.) (x:num0,y:int) = NearEqu(x,Int_e y)
    static member (<==.) (x:num0,y:double) = NearEqu(x,Dbl_e y)
    static member (<==.) (x:int,y:num0) = NearEqu(Int_e x, y)
    static member (<==.) (x:double,y:num0) = NearEqu(Dbl_e x,y)
    
    static member (<==/) (x:num0,y:num0) = NotEqu(x,y)
    static member (<==/) (x:num0,y:int) = NotEqu(x,Int_e y)
    static member (<==/) (x:num0,y:double) = NotEqu(x,Dbl_e y)
    static member (<==/) (x:int,y:num0) = NotEqu(Int_e x, y)
    static member (<==/) (x:double,y:num0) = NotEqu(Dbl_e x,y)
    
    static member (>.) (x:num0,y:num0) = Greater(x,y)
    static member (>.) (x:num0,y:int) = Greater(x,Int_e y)
    static member (>.) (x:num0,y:double) = Greater(x,Dbl_e y)
    static member (>.) (x:int,y:num0) = Greater(Int_e x, y)
    static member (>.) (x:double,y:num0) = Greater(Dbl_e x,y)
    
    static member (<.) (x:num0,y:num0) = Smaller(x,y)
    static member (<.) (x:num0,y:int) = Smaller(x,Int_e y)
    static member (<.) (x:num0,y:double) = Smaller(x,Dbl_e y)
    static member (<.) (x:int,y:num0) = Smaller(Int_e x, y)
    static member (<.) (x:double,y:num0) = Smaller(Dbl_e x,y)
    
    static member (>=.) (x:num0,y:num0) = GreaterEqu(x,y)
    static member (>=.) (x:num0,y:int) = GreaterEqu(x,Int_e y)
    static member (>=.) (x:num0,y:double) = GreaterEqu(x,Dbl_e y)
    static member (>=.) (x:int,y:num0) = GreaterEqu(Int_e x, y)
    static member (>=.) (x:double,y:num0) = GreaterEqu(Dbl_e x,y)
    
    static member (<=.) (x:num0,y:num0) = SmallerEqu(x,y)
    static member (<=.) (x:num0,y:int) = SmallerEqu(x,Int_e y)
    static member (<=.) (x:num0,y:double) = SmallerEqu(x,Dbl_e y)
    static member (<=.) (x:int,y:num0) = SmallerEqu(Int_e x, y)
    static member (<=.) (x:double,y:num0) = SmallerEqu(Dbl_e x,y)
    
    member this.sub(n:num0) = Var_e (this.str JavaScript  + "_{n}")
    
    member this.str(c:CodeLanguage) =
        match c,this with
        |_,Int_e x ->
            x.ToString()
        |_,Dbl_e x ->
            x.ToString()
        |_,Var_e x ->
            x
        |LaTeX,Vec_e x ->
            "\\boldsymbol{"+x.str c+"}"
        |JavaScript,Vec_e x ->
            x.str c
        |Text,Vec_e x ->
            "ベクトル"+x.str c
        |LaTeX,Mat_e x ->
            "\\mathrm{"+x.str c+"}"
        |JavaScript,Mat_e x ->
            x.str c
        |Text,Mat_e x ->
            "行列"+x.str c
        |Text,Num_e x ->
            "\\mathbb{"+x.str c+"}"
        |LaTeX,Num_e x ->
            x.str c
        |JavaScript,Num_e x ->
            x.str c
        |Text,Equ(x,y) ->
            x.str c + "イコール" + y.str c
        |_,Equ(x,y) ->
            x.str c + "=" + y.str c
        |Text,NearEqu(x,y) ->
            x.str c+"ニアリーイコール"+y.str c
        |_,NearEqu(x,y) ->
            x.str c+"\\simeq"+y.str c
        |Text,NotEqu(x,y) ->
            x.str c+"ノットイコール"+y.str c
        |_,NotEqu(x,y) ->
            x.str c+"\\neq"+y.str c
        |Text,Greater(x,y) ->
            x.str c+"大なり"+y.str c
        |_,Greater(x,y) ->
            x.str c+">"+y.str c
        |Text,Smaller(x,y) ->
            x.str c+"小なり"+y.str c
        |_,Smaller(x,y) ->
            x.str c+"<"+y.str c
        |Text,GreaterEqu(x,y) ->
            x.str c+"大なりイコール"+y.str c
        |LaTeX,GreaterEqu(x,y) ->
            x.str c+"\\geq"+y.str c
        |JavaScript,GreaterEqu(x,y) ->
            x.str c + ">=" + y.str c
        |Text,SmallerEqu(x,y) ->
            x.str c + "小なりイコール" + y.str c
        |LaTeX,SmallerEqu(x,y) ->
            x.str c + "\\leq" + y.str c
        |JavaScript,SmallerEqu(x,y) ->
            x.str c + "<=" + y.str c
        |Text,Mul(x,y) -> 
            x.str c + "×" + y.str c
        |LaTeX,Mul(x,y) -> 
            match x with
            |Add _ | Sub _ ->
                match y with
                |Add _ | Sub _ ->
                    "\\left(" + x.str c + "\\right)" + "\\left(" + y.str c + "\\right)"
                |_->
                    "\\left(" + x.str c + "\\right)"+y.str c 
            |_ ->
                match y with
                |Add _ | Sub _ ->
                    x.str c + "\\left("+y.str c  + "\\right)"
                |_-> 
                    x.str c  + y.str c 
        |JavaScript,Mul(x,y) -> 
            match x with
            |Add _ | Sub _ ->
                match y with
                |Add _ | Sub _ ->
                    "(" + x.str c + ")*(" + y.str c + ")"
                |_->
                    "(" + x.str c + ")*"+y.str c 
            |_ ->
                match y with
                |Add _ | Sub _ ->
                    x.str c + "*(" + y.str c  + ")"
                |_-> 
                    x.str c  + "*" + y.str c 
        |Text,Div (x,y) -> 
            x.str c + "÷" + y.str c 
        |LaTeX,Div (x,y) -> 
            "\\dfrac{" + x.str c + "}"+"{" + y.str c + "}" 
        |JavaScript,Div (x,y) -> 
            match x,y with
            |(Add _| Sub _),(Add _| Sub _) ->
                "("+x.str c + ")/(" + y.str c + ")"
            |(Add _| Sub _),_ ->
                "("+x.str c + ")/" + y.str c
            |_,(Add _| Sub _) ->
                "("+x.str c + ")/" + y.str c
            |_ ->
                x.str c + "/" + y.str c
        |Text,Add (x,y) -> 
            x.str c  + "プラス" + y.str c 
        |_,Add (x,y) -> 
            x.str c  + "+" + y.str c 
        |Text,Sub (x,y) -> 
            x.str c  + "マイナス" + y.str c 
        |_,Sub (x,y) -> 
            match y with
            |Add _|Sub _ ->
                x.str c  + "-(" + y.str c + ")"
            |_ ->
                x.str c  + "-" + y.str c 
        |Text,Pow (x,y) ->
            x.str c + "の"+y.str c + "乗"
        |LaTeX,Pow (x,y) ->
            x.str c + "^{"+y.str c + "}"
        |JavaScript,Pow (x,y) ->
            "Math.pow(" + x.str c + ","+y.str c + ")"
        |Text,Sin x -> 
            "サイン"+x.str c
        |LaTeX,Sin x -> 
            match x with
                |Add _ | Sub _ ->
                    "\\sin\\left("+x.str c + "\\right)"
                |_->
                    "\\sin{"+x.str c + "}"
        |JavaScript,Sin x -> 
            "Math.sin("+x.str c + ")"
        |Text,Cos x -> 
            "コサイン"+x.str c
        |LaTeX,Cos x -> 
            match x with
                |Add _ | Sub _ ->
                    "\\cos\\left("+x.str c + "\\right)"
                |_->
                    "\\cos{"+x.str c + "}"
        |_,Cos x -> 
            "Math.cos("+x.str c + ")"
        |Text,Tan x -> 
            "タンジェント"+x.str c
        |LaTeX,Tan x -> 
            match x with
                |Add _ | Sub _ ->
                    "\\tan\\left("+x.str c + "\\right)"
                |_->
                    "\\tan{"+x.str c + "}"
        |_,Tan x -> 
            "Math.tan("+x.str c + ")"
        |Text,Exp x ->
            "Eの"+x.str c + "乗"
        |LaTeX,Exp x ->
            "e^{"+x.str c + "}"
        |_,Exp x ->
            "Math.exp("+x.str c + ")"
        |Text,Log x -> 
            "ログ"+x.str c
        |LaTeX,Log x -> 
            match x with
                |Add _ | Sub _ ->
                    "\\log\\left("+x.str c + "\\right)"
                |_->
                    "\\log{"+x.str c + "}"
        |_,Log x -> 
            "Math.log("+x.str c + ")"
        |Text,Log10 x -> 
            "ログ"+x.str c
        |LaTeX,Log10 x -> 
            match x with
                |Add _ | Sub _ ->
                    "\\log_{10}\\left("+x.str c + "\\right)"
                |_->
                    "\\log_{10}{"+x.str c + "}"
        |_,Log10 x -> 
            "Math.log10("+x.str c + ")"
        |Text,Asin x -> 
            "アークサイン"+x.str c
        |LaTeX,Asin x -> 
            match x with
                |Add _ | Sub _ ->
                    "\\arcsin\\left("+x.str c + "\\right)"
                |_->
                    "\\arcsin{"+x.str c + "}"
        |_,Asin x -> 
            "Math.asin("+x.str c + ")"
        |Text,Acos x -> 
            "アークコサイン"+x.str c
        |LaTeX,Acos x -> 
            match x with
                |Add _ | Sub _ ->
                    "\\arccos\\left("+x.str c + "\\right)"
                |_->
                    "\\arccos{"+x.str c + "}"
        |_,Acos x -> 
            "Math.acos("+x.str c + ")"
        |Text,Atan x -> 
            "アークタンジェント"+x.str c
        |LaTeX,Atan x -> 
            match x with
                |Add _ | Sub _ ->
                    "\\arctan\\left("+x.str c + "\\right)"
                |_->
                    "\\arctan{"+x.str c + "}"
        |_,Atan x -> 
            "Math.atan("+x.str c + ")"
        |Text,Atan2 (x,y) -> 
            "アークタンジェント"+(y/x).str c
        |LaTeX,Atan2 (x,y) -> 
            match x with
                |Add _ | Sub _ ->
                    "\\arctan\\left("+(y/x).str c + "\\right)"
                |_->
                    "\\arctan{"+(y/x).str c + "}"
        |_,Atan2 (x,y) -> 
            "Math.atan2(" + y.str c + "," + y.str c + ")"
        |Text,Sqrt x -> 
            "ルート"+x.str c
        |LaTeX,Sqrt x -> 
            "\\sqrt{"+x.str c + "}"
        |_,Sqrt x -> 
            "Math.sqrt("+x.str c + ")"
        |Text,Abs x -> 
            "絶対値"+x.str c
        |LaTeX,Abs x -> 
            "\\left|"+x.str c + "\\right|"
        |_,Abs x -> 
            "Math.abs("+x.str c + ")"
