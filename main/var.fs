namespace Aqualis
    
    open System

    ///<summary>変数宣言</summary>
    type var () =
        
        ///<summary>複素数の初期値</summary>
        static member private init_z (re:double,im:double) =
            match pr.language with
            |Fortran ->
                "(" + pr.numFormat.DtoS re + "," + pr.numFormat.DtoS im+")"
            |C99 ->
                if im<0.0 then
                    pr.numFormat.DtoS re + "-uj*"+pr.numFormat.DtoS (abs im)
                else
                    pr.numFormat.DtoS re + "+uj*"+pr.numFormat.DtoS im
            |LaTeX ->
                "(" + pr.numFormat.DtoS re + (if im>=0.0 then "+" else "") + pr.numFormat.DtoS im + "\\mathrm{j})"
            |HTML ->
                "(" + pr.numFormat.DtoS re + (if im>=0.0 then "+" else "") + pr.numFormat.DtoS im + "\\mathrm{j})"
            |Python ->
                if im<0.0 then
                    pr.numFormat.DtoS re + "-" + pr.numFormat.DtoS (abs im) + "i"
                else
                    pr.numFormat.DtoS re + "+" + pr.numFormat.DtoS im + "i"
            |JavaScript ->
                "(" + pr.numFormat.DtoS re + (if im>=0.0 then "+" else "") + pr.numFormat.DtoS im + ")"
            |PHP ->
                "(" + pr.numFormat.DtoS re + (if im>=0.0 then "+" else "") + pr.numFormat.DtoS im + ")"
            |Numeric ->
                "(" + pr.numFormat.DtoS re + (if im>=0.0 then "+" else "") + pr.numFormat.DtoS im + ")"
                
        ///<summary>整数型配列の初期値</summary>
        static member private init_i1 (v:int list) =
            let join (s:list<int>) = String.Join(",", s |> List.map (fun v -> pr.numFormat.ItoS v))
            match pr.language with
            |Fortran -> "(/" + join v + "/)"
            |C99 -> "{" + join v + "}"
            |LaTeX -> "{" + join v + "}"
            |HTML -> "{" + join v + "}"
            |Python -> "[" + join v + "]"
            |JavaScript -> "[" + join v + "]"
            |PHP -> "[" + join v + "]"
            |Numeric -> "{" + join v + "}"
            
        ///<summary>倍精度浮動小数点型配列の初期値</summary>
        static member private init_d1 (v:double list) =
            let join (s:list<double>) = String.Join(",", s |> List.map (fun v -> pr.numFormat.DtoS v))
            match pr.language with
            |Fortran -> "(/" + join v + "/)"
            |C99 -> "{" + join v + "}"
            |LaTeX -> "{" + join v + "}"
            |HTML -> "{" + join v + "}"
            |Python -> "[" + join v + "]"
            |JavaScript -> "[" + join v + "]"
            |PHP -> "[" + join v + "]"
            |Numeric -> "{" + join v + "}"
            
        ///<summary>複素数型配列の初期値</summary>
        static member private init_z1 (v:(double*double) list) =
            let join (s:list<double*double>) = String.Join(",", s |> List.map (fun v -> var.init_z v))
            match pr.language with
            |Fortran -> "(/" + join v + "/)"
            |C99 -> "{" + join v + "}"
            |LaTeX -> "{" + join v + "}"
            |HTML -> "{" + join v + "}"
            |Python -> "[" + join v + "]"
            |JavaScript -> "[" + join v + "]"
            |PHP -> "[" + join v + "]"
            |Numeric -> "{" + join v + "}"
            
        ///<summary>バイト型変数</summary>
        ///<param name="name">変数名</param>
        static member b0 (name:string) = 
            pr.var.setUniqVarWarning(It 1,A0,name,"")
            num0(Var(It 1,name,NaN))
            
        ///<summary>整数型変数</summary>
        ///<param name="name">変数名</param>
        static member i0 (name:string) = 
            pr.var.setUniqVarWarning(It 4,A0,name,"")
            num0(Var(It 1,name,NaN))
            
        ///<summary>倍精度浮動小数点型変数</summary>
        ///<param name="name">変数名</param>
        static member d0 (name:string) =
            pr.var.setUniqVarWarning(Dt,A0,name,"")
            num0(Var(Dt,name,NaN))
            
        ///<summary>複素数型変数</summary>
        ///<param name="name">変数名</param>
        static member z0 (name:string) = 
            pr.var.setUniqVarWarning(Zt,A0,name,"")
            num0(Var(Zt,name,NaN))
            
        ///<summary>整数型1次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数</param>
        static member i1(name:string,size1:int) = 
            num1(It 4,A1 size1,name,"")
            
        ///<summary>整数型可変長1次元配列</summary>
        ///<param name="name">変数名</param>
        static member i1(name:string) =
            num1(It 4,A1 0,name,"")
            
        ///<summary>倍精度浮動小数点型1次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数</param>
        static member d1(name:string,size1:int) = 
            num1(Dt,A1 size1,name,"")
            
        ///<summary>倍精度浮動小数点型可変長1次元配列</summary>
        ///<param name="name">変数名</param>
        static member d1(name:string) = 
            num1(Dt,A1 0,name,"")
            
        ///<summary>複素数型1次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数</param>
        static member z1(name:string,size1:int) = 
            num1(Zt,A1 size1,name,"")
            
        ///<summary>複素数型可変長1次元配列</summary>
        ///<param name="name">変数名</param>
        static member z1(name:string) = 
            num1(Zt,A1 0,name,"")
            
        ///<summary>整数型2次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        static member i2(name:string,size1:int,size2:int) = 
            num2(It 4,A2(size1,size2),name,"")
            
        ///<summary>整数型可変長2次元配列</summary>
        ///<param name="name">変数名</param>
        static member i2(name:string) = 
            num2(It 4,A2(0,0),name,"")
            
        ///<summary>倍精度浮動小数点型2次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        static member d2(name:string,size1:int,size2:int) = 
            num2(Dt,A2(size1,size2),name,"")
            
        ///<summary>倍精度浮動小数点型可変長2次元配列</summary>
        ///<param name="name">変数名</param>
        static member d2(name:string) = 
            num2(Dt,A2(0,0),name,"")
            
        ///<summary>複素数型2次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        static member z2(name:string,size1:int,size2:int) = 
            num2(Zt,A2(size1,size2),name,"")
            
        ///<summary>複素数型可変長2次元配列</summary>
        ///<param name="name">変数名</param>
        static member z2(name:string) = 
            num2(Zt,A2(0,0),name,"")
            
        ///<summary>整数型3次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        ///<param name="size3">要素数3</param>
        static member i3(name:string,size1:int,size2:int,size3:int) = 
            num3(It 4,A3(size1,size2,size3),name,"")
            
        ///<summary>整数型可変長3次元配列</summary>
        ///<param name="name">変数名</param>
        static member i3(name:string) = 
            num3(It 4,A3(0,0,0),name,"")
            
        ///<summary>倍精度浮動小数点型3次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        ///<param name="size3">要素数3</param>
        static member d3(name:string,size1:int,size2:int,size3:int) = 
            num3(Dt,A3(size1,size2,size3),name,"")
            
        ///<summary>倍精度浮動小数点型可変長3次元配列</summary>
        ///<param name="name">変数名</param>
        static member d3(name:string) = 
            num3(Dt,A3(0,0,0),name,"")
            
        ///<summary>複素数型3次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        ///<param name="size3">要素数3</param>
        static member z3(name:string,size1:int,size2:int,size3:int) = 
            num3(Zt,A3(size1,size2,size3),name,"")
            
        ///<summary>複素数型可変長3次元配列</summary>
        ///<param name="name">変数名</param>
        static member z3(name:string) = 
            num3(Zt,A3(0,0,0),name,"")
            
        ///<summary>整数型定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member ip0(name:string,v) = 
            pr.var.setUniqVarWarning(It 4,A0,name,(pr.numFormat.ItoS v))
            num0(Var(It 4, name,NaN))
            
        ///<summary>倍精度浮動小数点型定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member dp0(name:string,v) = 
            pr.var.setUniqVarWarning(Dt,A0,name,(pr.numFormat.DtoS v))
            num0(Var(Dt, name,NaN))
            
        ///<summary>整数型定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member internal ip0_noWarning(name:string,v) = 
            pr.var.setUniqVar(It 4,A0,name,pr.numFormat.ItoS v)
            num0(Var(It 4, name,NaN))
            
        ///<summary>倍精度浮動小数点型定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member internal dp0_noWarning(name:string,v) = 
            pr.var.setUniqVar(Dt,A0,name,pr.numFormat.DtoS v)
            num0(Var(Dt, name,NaN))
            
        ///<summary>複素数型定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member zp0(name,(v:double*double)) =
            let (v_re,v_im) = v
            pr.var.setUniqVarWarning(Zt,A0,name,var.init_z (v_re,v_im))
            num0(Var(Zt, name,NaN))
            
        ///<summary>整数型1次元配列定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member ip1(name:string,v:int list) =
            pr.var.setUniqVarWarning(It 4,A1 v.Length,name,var.init_i1 v)
            num1(It 4,Var1(A1 v.Length,name))
            
        ///<summary>倍精度浮動小数点型1次元配列定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member dp1(name:string,v:double list) = 
            pr.var.setUniqVarWarning(Dt,A1 v.Length,name,var.init_d1 v)
            num1(Dt,Var1(A1 v.Length,name))
            
        ///<summary>複素数型1次元配列定数</summary>
        ///<param name="name">変数名</param>
        ///<param name="v">定数</param>
        static member zp1(name:string,v:(double*double) list) =
            pr.var.setUniqVarWarning(Zt,A0,name,var.init_z1 v)
            num1(Zt,Var1(A1 v.Length,name))
            
        ///<summary>整数型変数</summary>
        ///<param name="name">変数名</param>
        static member n0(e:Etype,name:string) = 
            match e with 
            |Nt |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n0)" <| e.ToString()
            |_ -> ()
            pr.var.setUniqVarWarning(e,A0,name,"")
            num0(Var(e,name,NaN))
            
        ///<summary>整数型1次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数</param>
        static member n1(e:Etype,name:string,size1:int) = 
            match e with 
            |Nt |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n1)" <| e.ToString()
            |_ -> ()
            num1(e,Var1(A1 size1,name))
            
        ///<summary>整数型可変長1次元配列</summary>
        ///<param name="name">変数名</param>
        static member n1(e:Etype,name:string) =
            match e with 
            |Nt |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n1)" <| e.ToString()
            |_ -> ()
            num1(e,Var1(A1 0,name))
            
        ///<summary>整数型2次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        static member n2(e:Etype,name:string,size1:int,size2:int) = 
            match e with 
            |Nt |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n2)" <| e.ToString()
            |_ -> ()
            num2(e,Var2(A2(size1,size2),name))
            
        ///<summary>整数型可変長2次元配列</summary>
        ///<param name="name">変数名</param>
        static member n2(e:Etype,name:string) = 
            match e with 
            |Nt |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n2)" <| e.ToString()
            |_ -> ()
            num2(e,Var2(A2(0,0),name))
            
        ///<summary>整数型3次元配列</summary>
        ///<param name="name">変数名</param>
        ///<param name="size1">要素数1</param>
        ///<param name="size2">要素数2</param>
        ///<param name="size3">要素数3</param>
        static member n3(e:Etype,name:string,size1:int,size2:int,size3:int) = 
            match e with 
            |Nt |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n3)" <| e.ToString()
            |_ -> ()
            num3(e,Var3(A3(size1,size2,size3),name))
            
        ///<summary>整数型可変長3次元配列</summary>
        ///<param name="name">変数名</param>
        static member n3(e:Etype,name:string) = 
            match e with 
            |Nt |Structure _ -> printfn "%s: 変数を生成できない型です(gvar.n3)" <| e.ToString()
            |_ -> ()
            num3(e,Var3(A3(0,0,0),name))