namespace Aqualis
    
    type bool0(x:expr) =
        member this.Expr with get() = x
        member this.code with get() = x.eval pr
        
    ///<summary>数値と文字列の結合</summary>
    type exprString = 
        |Str of string
        |Nvr of expr
        |NSL of list<exprString>
        
        static member ( ++ ) (x:exprString,y:string) = 
            match x with
            |Str x -> NSL[Str x; Str y]
            |Nvr x -> NSL[Nvr x; Str y]
            |NSL x -> NSL(x@[Str y])
            
    ///<summary>変数（数値データ）クラス</summary>
    type num0(x:expr) =
        
        member this.Expr with get() = x
        
        member this.etype with get() = x.etype
        
        member this.code with get() = x.eval pr
        
        ///<summary>優先度の高い型を選択</summary>
        static member ( %% ) (x:num0,y:num0) = 
            match x.etype,y.etype with
            |Zt,(Zt|Dt|It _) -> Zt
            |(Zt|Dt|It _),Zt -> Zt
            |Dt,(Dt|It _) -> Dt
            |(Dt|It _),Dt -> Dt
            |It a,It b -> It (if a>b then a else b)
            |_ -> Nt
            
        static member ( ++ ) (x:string,y:num0) = NSL[Str x; Nvr y.Expr]
        static member ( ++ ) (x:num0,y:string) = NSL[Nvr x.Expr; Str y]
        static member ( ++ ) (x:num0,y:num0) = NSL[Nvr x.Expr; Nvr y.Expr]
        static member ( ++ ) (x:exprString,y:num0) = 
            match x with
            |Str x -> NSL[Str x; Nvr y.Expr]
            |Nvr x -> NSL[Nvr x; Nvr y.Expr]
            |NSL x -> NSL(x@[Nvr y.Expr])
            
        static member ( ++ ) (x:string,y:exprString) = 
            match y with
            |Str y -> NSL[Str x; Str y]
            |Nvr y -> NSL[Str x; Nvr y]
            |NSL y -> NSL([Str x]@y)
            
        static member ( ++ ) (x:num0,y:exprString) = 
            match y with
            |Str y -> NSL[Nvr x.Expr; Str y]
            |Nvr y -> NSL[Nvr x.Expr; Nvr y]
            |NSL y -> NSL([Nvr x.Expr]@y)
        
        ///<summary>負号</summary>
        static member ( ~- ) (x:num0) = num0(Inv(x.etype,x.Expr))
        
        ///<summary>加算</summary>
        static member ( + ) (x:num0,y:num0) = num0(Add(x%%y, x.Expr, y.Expr))
        static member ( + ) (x:num0,y:double) = x + num0(Dbl y)
        static member ( + ) (x:num0,y:int) = x + num0(Int y)
        static member ( + ) (x:double,y:num0) = num0(Dbl x) + y
        static member ( + ) (x:int,y:num0) = num0(Int x) + y
        
        ///<summary>減算</summary>
        static member ( - ) (x:num0,y:num0) = num0(Sub(x%%y, x.Expr, y.Expr))
        static member ( - ) (x:num0,y:double) = x-num0(Dbl y)
        static member ( - ) (x:num0,y:int) = x-num0(Int y)
        static member ( - ) (x:double,y:num0) = num0(Dbl x)-y
        static member ( - ) (x:int,y:num0) = num0(Int x)-y
        
        ///<summary>乗算</summary>
        static member ( * ) (x:num0,y:num0) = num0(Mul(x%%y, x.Expr, y.Expr))
        static member ( * ) (x:num0,y:double) = x*num0(Dbl y)
        static member ( * ) (x:num0,y:int) = x*num0(Int y)
        static member ( * ) (x:double,y:num0) = num0(Dbl x)*y
        static member ( * ) (x:int,y:num0) = num0(Int x)*y
        
        ///<summary>除算</summary>
        static member ( / ) (x:num0,y:num0) = num0(Div(Dt%%x.etype%%y.etype, x.Expr, y.Expr))
        static member ( / ) (x:num0,y:double) = x/num0(Dbl y)
        static member ( / ) (x:num0,y:int) = x/num0(Dbl(double y))
        static member ( / ) (x:double,y:num0) = num0(Dbl x)/y
        static member ( / ) (x:int,y:num0) = num0(Dbl(double x))/y
        
        ///<summary>整数同士の除算(剰余無視)</summary>
        static member ( ./ ) (x:num0,y:num0) = num0(Div(It 4, x.Expr, y.Expr))
        static member ( ./ ) (x:num0,y:int) = x./num0(Int y)
        static member ( ./ ) (x:int,y:num0) = num0(Int x)./y
        
        ///<summary>剰余</summary>
        static member ( % ) (x:num0,y:num0) = num0(Mod(It 4, x.Expr, y.Expr))
        static member ( % ) (x:num0,y:int) = x % num0(Int y)
        static member ( % ) (x:int,y:num0) = num0(Int x) % y
        
        ///<summary>累乗</summary>
        static member powr(x:num0, y:num0) = num0(Pow(x%%y, x.Expr, y.Expr))
        static member ( .** ) (x:num0, y:num0) = num0(Pow(x%%y, x.Expr, y.Expr))
        
        ///<summary>等号</summary>
        static member (.=) (x:num0,y:num0) = bool0(Eq(x.Expr,y.Expr))
        static member (.=) (x:int,y:num0) = num0(Int x) .= y
        static member (.=) (x:double,y:num0) = num0(Dbl x) .= y
        static member (.=) (x:num0,y:int) = x .= num0(Int y)
        static member (.=) (x:num0,y:double) = x .= num0(Dbl y)
        ///<summary>不等号</summary>
        static member (.=/) (x:num0,y:num0) = bool0(NEq(x.Expr,y.Expr))
        static member (.=/) (x:int,y:num0) = num0(Int x) .=/ y
        static member (.=/) (x:double,y:num0) = num0(Dbl x) .=/ y
        static member (.=/) (x:num0,y:int) = x .=/ num0(Int y)
        static member (.=/) (x:num0,y:double) = x .=/ num0(Dbl y)
        ///<summary>比較（より小）</summary>
        static member (.<) (x:num0,y:num0) = bool0(Less(x.Expr,y.Expr))
        static member (.<) (x:int,y:num0) = num0(Int x) .< y
        static member (.<) (x:double,y:num0) = num0(Dbl x) .< y
        static member (.<) (x:num0,y:int) = x .< num0(Int y)
        static member (.<) (x:num0,y:double) = x .< num0(Dbl y)
        static member (.<) (v1:bool0,v2:num0) = 
            match v1.Expr with
            |Less(u1,u2) -> bool0(AND[Less(u1,u2);Less(u2,v2.Expr)])
            |LessEq(u1,u2) -> bool0(AND[Less(u1,u2);Less(u2,v2.Expr)])
            |Greater(u1,u2) ->bool0(AND[Less(u1,u2);Less(u2,v2.Expr)])
            |GreaterEq(u1,u2) ->bool0(AND[Less(u1,u2);Less(u2,v2.Expr)])
            |AND lst ->
                match lst with
                |[] -> bool0 NaN
                |_ ->
                    match lst[lst.Length-1] with
                    |Less(_,u2) |LessEq(_,u2) |Greater(_,u2) |GreaterEq(_,u2) -> 
                        bool0(AND <| lst@[Less(u2,v2.Expr)])
                    |_ -> bool0 NaN
            |_ ->
                bool0 NaN
                
        ///<summary>比較（以下）</summary>
        static member (.<=) (x:num0,y:num0) = bool0(LessEq(x.Expr,y.Expr))
        static member (.<=) (x:int,y:num0) = num0(Int x) .<= y
        static member (.<=) (x:double,y:num0) = num0(Dbl x) .<= y
        static member (.<=) (x:num0,y:int) = x .<= num0(Int y)
        static member (.<=) (x:num0,y:double) = x .<= num0(Dbl y)
        static member (.<=) (v1:bool0,v2:num0) = 
            match v1.Expr with
            |Less(u1,u2) -> bool0(AND[Less(u1,u2);LessEq(u2,v2.Expr)])
            |LessEq(u1,u2) -> bool0(AND[Less(u1,u2);LessEq(u2,v2.Expr)])
            |Greater(u1,u2) ->bool0(AND[Less(u1,u2);LessEq(u2,v2.Expr)])
            |GreaterEq(u1,u2) ->bool0(AND[Less(u1,u2);LessEq(u2,v2.Expr)])
            |AND lst ->
                match lst with
                |[] -> bool0 NaN
                |_ ->
                    match lst[lst.Length-1] with
                    |Less(_,u2) |LessEq(_,u2) |Greater(_,u2) |GreaterEq(_,u2) -> 
                        bool0(AND <| lst@[LessEq(u2,v2.Expr)])
                    |_ -> bool0 NaN
            |_ ->
                bool0 NaN
                
        ///<summary>比較（より大）</summary>
        static member (.>) (x:num0,y:num0) = bool0(Greater(x.Expr,y.Expr))
        static member (.>) (x:int,y:num0) = num0(Int x) .> y
        static member (.>) (x:double,y:num0) = num0(Dbl x) .> y
        static member (.>) (x:num0,y:int) = x .> num0(Int y)
        static member (.>) (x:num0,y:double) = x .> num0(Dbl y)
        static member (.>) (v1:bool0,v2:num0) = 
            match v1.Expr with
            |Less(u1,u2) -> bool0(AND[Less(u1,u2);Greater(u2,v2.Expr)])
            |LessEq(u1,u2) -> bool0(AND[Less(u1,u2);Greater(u2,v2.Expr)])
            |Greater(u1,u2) ->bool0(AND[Less(u1,u2);Greater(u2,v2.Expr)])
            |GreaterEq(u1,u2) ->bool0(AND[Less(u1,u2);Greater(u2,v2.Expr)])
            |AND lst ->
                match lst with
                |[] -> bool0 NaN
                |_ ->
                    match lst[lst.Length-1] with
                    |Less(_,u2) |LessEq(_,u2) |Greater(_,u2) |GreaterEq(_,u2) -> 
                        bool0(AND <| lst@[Greater(u2,v2.Expr)])
                    |_ -> bool0 NaN
            |_ ->
                bool0 NaN
        ///<summary>比較（以上）</summary>
        static member (.>=) (x:num0,y:num0) = bool0(GreaterEq(x.Expr,y.Expr))
        static member (.>=) (x:int,y:num0) = num0(Int x) .>= y
        static member (.>=) (x:double,y:num0) = num0(Dbl x) .>= y
        static member (.>=) (x:num0,y:int) = x .>= num0(Int y)
        static member (.>=) (x:num0,y:double) = x .>= num0(Dbl y)
        static member (.>=) (v1:bool0,v2:num0) = 
            match v1.Expr with
            |Less(u1,u2) -> bool0(AND[Less(u1,u2);GreaterEq(u2,v2.Expr)])
            |LessEq(u1,u2) -> bool0(AND[Less(u1,u2);GreaterEq(u2,v2.Expr)])
            |Greater(u1,u2) ->bool0(AND[Less(u1,u2);GreaterEq(u2,v2.Expr)])
            |GreaterEq(u1,u2) ->bool0(AND[Less(u1,u2);GreaterEq(u2,v2.Expr)])
            |AND lst ->
                match lst with
                |[] -> bool0 NaN
                |_ ->
                    match lst[lst.Length-1] with
                    |Less(_,u2) |LessEq(_,u2) |Greater(_,u2) |GreaterEq(_,u2) -> 
                        bool0(AND <| lst@[GreaterEq(u2,v2.Expr)])
                    |_ -> bool0 NaN
            |_ ->
                bool0 NaN
                
        ///<summary>代入</summary>
        static member (<==) (x:num0,y:num0) = expr.subst x.Expr y.Expr pr
        static member (<==) (x:num0,y:int) = x <== num0(Int y)
        static member (<==) (x:num0,y:double) = x <== num0(Dbl y)
        member this.clear() = this <== 0
        
        ///<summary>等式(TeX、HTMLのみ)</summary>
        static member (===) (x:num0,y:num0) = expr.equiv x.Expr y.Expr pr
        static member (===) (x:num0,y:int) = x === num0(Int y)
        static member (===) (x:num0,y:double) = x === num0(Dbl y)
        
        ///<summary>等号揃等式(TeX、HTMLのみ)</summary>
        static member (=|=) (x:num0,y:num0) = expr.equivAlign x.Expr y.Expr pr
        static member (=|=) (x:num0,y:int) = x =|= num0(Int y)
        static member (=|=) (x:num0,y:double) = x =|= num0(Dbl y)
