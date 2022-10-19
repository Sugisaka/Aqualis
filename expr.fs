(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open System.IO
    open System.Text
    open Aqualis_base

    ///<summary>数値型</summary>
    type num0 =
        |Int_e of int
        |Dbl_e of double
        |Str_e of string
        |Var of Etype*string*(num0 list)
        |Par of Etype*num0
        |Inv of Etype*num0
        |Re  of num0
        |Im  of num0
        |Add of Etype*(num0 list)
        |Sub of Etype*num0*num0
        |Mul of Etype*num0*num0
        |Div of Etype*num0*num0
        |Pow of Etype*num0*num0
        |Mod of num0*num0
        |Exp of Etype*num0
        |Sin of Etype*num0
        |Cos of Etype*num0
        |Tan of Etype*num0
        |Asin of Etype*num0
        |Acos of Etype*num0
        |Atan of Etype*num0
        |Atan2 of Etype*num0*num0
        |Abs of Etype*num0
        |Log of Etype*num0
        |Log10 of Etype*num0
        |Sqrt of Etype*num0
        |Floor of Etype*num0
        |Ceiling of Etype*num0
        |ToInt of num0
        |ToDbl of num0
        |Conj of num0
        |Idx1 of Etype*num0*num0
        |Idx2 of Etype*num0*num0*num0
        |Idx3 of Etype*num0*num0*num0*num0
        |Code of Etype*string*(num0 list)
        |NaN

        ///<summary>Codeから型、コード、一時変数を取得</summary>
        member this.etype
          with get() =
              match this with
                |Int_e _ -> It 4
                |Dbl_e _ -> Dt
                |Str_e _ -> Structure("string")
                |Var(t,_,_) -> t
                |Par(t,_) -> t
                |Inv(t,_) -> t
                |Re _ -> Dt
                |Im _ -> Dt
                |Add(t,_) -> t
                |Sub(t,_,_) -> t
                |Mul(t,_,_) -> t
                |Div(t,_,_) -> t
                |Pow(t,_,_) -> t
                |Mod(_,_) -> It 4
                |Exp(t,_) -> t
                |Sin(t,_) -> t
                |Cos(t,_) -> t
                |Tan(t,_) -> t
                |Asin(t,_) -> t
                |Acos(t,_) -> t
                |Atan(t,_) -> t
                |Atan2(t,_,_) -> t
                |Abs(t,_) -> t
                |Log(t,_) -> t
                |Log10(t,_) -> t
                |Sqrt(t,_) -> t
                |Floor(t,_) -> t
                |Ceiling(t,_) -> t
                |ToInt(_) -> It 4
                |ToDbl(_) -> Dt
                |Conj(_) -> Zt
                |Idx1(t,_,_) -> t
                |Idx2(t,_,_,_) -> t
                |Idx3(t,_,_,_,_) -> t
                |Code(t,_,_) -> t
                |NaN -> Nt
                
        ///<summary>二つの変数から優先度の高い型を選択（Nt→Zt→Dt→It）</summary>
        static member ptype(t1:Etype,t2:Etype) =
            match t1,t2 with
              |Nt,_|_,Nt -> Nt
              |Zt,_|_,Zt -> Zt
              |Dt,_|_,Dt -> Dt
              |_ -> It 4
        ///<summary>二つの変数から優先度の高い型を選択（Nt→Zt→Dt→It）</summary>
        static member ptype(t1:num0,t2:num0) = num0.ptype(t1.etype,t2.etype)
        ///<summary>二つの変数から優先度の高い型を選択（Nt→Zt→Dt→It）</summary>
        static member ptype(t1:num0,t2:Etype) = num0.ptype(t1.etype,t2)
        ///<summary>二つの変数から優先度の高い型を選択（Nt→Zt→Dt→It）</summary>
        static member ptype(t1:Etype,t2:num0) = num0.ptype(t1,t2.etype)
        
        ///<summary>虚数単位</summary>
        static member private uj =
            Var(Zt,"uj",[])
    
        /// <summary>
        /// この変数を関数内変数に変換
        /// </summary>
        /// <param name="cm">コメント</param>
        member this.farg (cm:string) =
            let p = p.param
            fun code -> p.addarg (this.etype,A0,this.name,cm) <| fun (typ,_,n) -> code(Var(typ,n,[]))
            
        ///<summary>Codeから型、コード、一時変数を取得</summary>
        member internal this.str
          with get() =
            match this with
              |Var(a,b,c) -> (a,b,c)
              |Code(a,b,c) -> (a,b,c)
              |_ -> (Nt,"",[])
              
        ///<summary>変数名、式を文字列で取得</summary>
        member this.name
          with get() =
            let x = this.code
            match p.lang with
              |H ->
                let chtml (s:string) =
                    let rec conv (ss:string) lst =
                      match lst with
                        |(a:string,b:string)::lst0 ->
                          conv (ss.Replace(a,b)) lst0
                        |[] -> ss
                    conv s [("&","&amp;");("<","&lt;");(">","&gt;");("\"","&quot;");("\'","&#39;");("\s","&nbsp;")]
                match x with
                  |Var(_,b,_) -> b
                  |Code(Structure("string"),b,_) -> "<mtext>&quot;" + (chtml b) + "&quot;</mtext>"
                  |Code(_,b,_) -> b
                  |_ -> ""
              |_ ->
                match x with
                  |Var(_,b,_) -> b
                  //|Code(Structure("string"),b,_) -> "\"" + b + "\""
                  |Code(_,b,_) -> b
                  |_ -> ""
              
        ///<summary>負号</summary>
        static member (~-) (x:num0) =
            match x with
              |Int_e 0|Dbl_e 0.0 -> x
              |Int_e v when v<0   -> (Int_e -v)
              |Dbl_e v when v<0.0 -> (Dbl_e -v)
              |Inv(_,v) -> v
              |Add(_,_) -> -Par(x.etype,x)
              |Sub(t,v1,v2) -> Sub(t,v2,v1)
              |_ -> Inv(x.etype,x)
    
        ///<summary>加算</summary>
        static member (+) (x:num0,y:num0) : num0 =
            match p.lang,x,y with
              (* x+0 *)
              |_,_,Int_e 0|_,_,Dbl_e 0.0 -> x
              (* 0+y *)
              |_,Int_e 0,_|_,Dbl_e 0.0,_ -> y
              (* (-x)+(-y) *)
              |_,Inv(_,v1),Inv(_,v2) -> -(v1+v2)
              (* x+(-y) *)
              |_,_,Inv(_,v2) -> x-v2
              (* (-x)+y *)
              |_,Inv(_,v1),_ -> y-v1
              (* x+[整数定数] *)
              |_,_,Int_e v2 when v2<0   -> x-Int_e(-v2)
              (* x+[小数定数] *)
              |_,_,Dbl_e v2 when v2<0.0 -> x-Dbl_e(-v2)
              (* [整数定数]+[整数定数] *)
              |(F|C89|C99|NL),Int_e v1,Int_e v2 -> Int_e(v1+v2)
              (* [整数定数]+[小数定数] *)
              |(F|C89|C99|NL),Int_e v1,Dbl_e v2 -> Dbl_e((double v1)+v2)
              (* [小数定数]+[整数定数] *)
              |(F|C89|C99|NL),Dbl_e v1,Int_e v2 -> Dbl_e(v1+(double v2))
              (* [小数定数]+[小数定数] *)
              |(F|C89|C99|NL),Dbl_e v1,Dbl_e v2 -> Dbl_e(v1+v2)
              (* (x1+x2+…)+(y1+y2+…) *)
              |_,Add(_,v1),Add(_,v2) -> Add(num0.ptype(x.etype,y.etype),v1@v2)
              (* (x1-x2)+(y1+y2+…) *)
              |_,Sub(_,u1,u2),Add(t,v2) -> Add(t,[u1;-u2;]@v2)
              (* (x1+x2+…)+(y1-y2) *)
              |_,Add(t,v1),Sub(_,u1,u2) -> Add(t,v1@[u1;-u2])
              (* x+(y1+y2+…) *)
              |_,_,Add(t,v2) -> Add(t,x::v2)
              (* (x1+x2+…)+y *)
              |_,Add(t,v1),_ -> Add(t,v1@[y])
              (* (x1-x2)+(y1-y2) *)
              |_,Sub(t1,v1,v2),Sub(t2,u1,u2) -> Add(num0.ptype(t1,t2),[v1;-v2;u1;-u2])
              (* x+(y1+y2+…) *)
              |_,_,Sub(_,v1,v2) -> Add(num0.ptype(x.etype,y.etype),[x;v1;-v2])
              (* (x1-x2)+y *)
              |_,Sub(_,v1,v2),_ -> Add(num0.ptype(x.etype,y.etype),[v1;-v2;y])
              (* x+y *)
              |_ -> Add(num0.ptype(x.etype,y.etype),[x;y])
              
        ///<summary>加算</summary>
        static member (+) (x:num0,y:int) = x+(Int_e y)
        
        ///<summary>加算</summary>
        static member (+) (x:int,y:num0) = (Int_e x)+y
        
        ///<summary>加算</summary>
        static member (+) (x:num0,y:double) = x+(Dbl_e y)
        
        ///<summary>加算</summary>
        static member (+) (x:double,y:num0) = (Dbl_e x)+y
        
        ///<summary>加算</summary>
        static member (+) ((re:double,im:double),y:num0) =
            (re+num0.uj*(Dbl_e im))+y
        
        ///<summary>加算</summary>
        static member (+) (x:num0,(re:double,im:double)) =
            x+(re+num0.uj*(Dbl_e im))
        
        ///<summary>減算</summary>
        static member (-) (x:num0,y:num0) : num0 =
            match p.lang,x,y with
              (* x-0 *)
              |_,_,Int_e 0|_,_,Dbl_e 0.0 -> x
              (* 0-y *)
              |_,Int_e 0,_|_,Dbl_e 0.0,_ -> -y //Inv(y.etype,y)
              (* (-x)-(-y) *)
              |_,Inv(_,v1),Inv(_,v2) -> v2-v1
              (* x-(-y) *)
              |_,_,Inv(_,v2) -> x+v2
              (* (-x)-y *)
              |_,Inv(_,v1),_ -> -(y+v1)
              (* x-[整数定数] *)
              |_,_,Int_e v2 when v2<0   -> x+Int_e(-v2)
              (* x-[小数定数] *)
              |_,_,Dbl_e v2 when v2<0.0 -> x+Dbl_e(-v2)
              (* [整数定数]-[整数定数] *)
              |(F|C89|C99|NL),Int_e v1,Int_e v2 -> Int_e(v1-v2)
              (* [整数定数]-[小数定数] *)
              |(F|C89|C99|NL),Int_e v1,Dbl_e v2 -> Dbl_e((double v1)-v2)
              (* [小数定数]-[整数定数] *)
              |(F|C89|C99|NL),Dbl_e v1,Int_e v2 -> Dbl_e(v1-(double v2))
              (* [小数定数]-[小数定数] *)
              |(F|C89|C99|NL),Dbl_e v1,Dbl_e v2 -> Dbl_e(v1-v2)
              (* (x1+x2+…)-(y1+y2+…) *)
              |_,Add(_,v1),Add(_,v2) -> Add(num0.ptype(x.etype,y.etype),v1@(v2|>List.map(fun v -> -v)))
              (* (x1-x2)-(y1+y2+…) *)
              |_,Sub(_,u1,u2),Add(t,v2) -> Add(t,[u1;-u2;]@(v2|>List.map(fun v -> -v)))
              (* (x1+x2+…)-(y1-y2) *)
              |_,Add(t,v1),Sub(_,u1,u2) -> Add(t,v1@[-u1;u2])
              (* x-(y1+y2+…) *)
              |_,_,Add(t,v2) -> Add(t,x::(v2|>List.map(fun v -> -v)))
              (* (x1+x2+…)-y *)
              |_,Add(t,v1),_ -> Add(t,v1@[-y])
              (* (x1-x2)-(y1-y2) *)
              |_,Sub(t1,v1,v2),Sub(t2,u1,u2) -> Add(num0.ptype(t1,t2),[v1;-v2;-u1;u2])
              (* x-(y1-y2) *)
              |_,_,Sub(_,v1,v2) -> Add(num0.ptype(x,y),[x;-v1;v2])
              (* (x1-x2)-y *)
              |_,Sub(_,v1,v2),_ -> Add(num0.ptype(x,y),[v1;-v2;-y])
              (* x-y *)
              |_ -> Sub(num0.ptype(x.etype,y.etype),x,y)
    
        ///<summary>減算</summary>
        static member (-) (x:num0,y:int) = x-(Int_e y)
        
        ///<summary>減算</summary>
        static member (-) (x:int,y:num0) = (Int_e x)-y
        
        ///<summary>減算</summary>
        static member (-) (x:num0,y:double) = x-(Dbl_e y)
        
        ///<summary>減算</summary>
        static member (-) (x:double,y:num0) = (Dbl_e x)-y
        
        ///<summary>減算</summary>
        static member (-) ((re:double,im:double),y:num0) =
            (re+num0.uj*(Dbl_e im))-y
        
        ///<summary>減算</summary>
        static member (-) (x:num0,(re:double,im:double)) = 
            x-(re+num0.uj*(Dbl_e im))
        
        ///<summary>乗算</summary>
        static member (*) (x:num0,y:num0) : num0 =
            match p.lang,x,y with
              (* x*0 *)
              |_,Int_e 0,_|_,Dbl_e 0.0,_|_,_,Int_e 0|_,_,Dbl_e 0.0 -> Int_e 0
              (* 1*y *)
              |_,Int_e 1,_|_,Dbl_e 1.0,_ -> y
              (* x*1 *)
              |_,_,Int_e 1|_,_,Dbl_e 1.0 -> x
              (* [負の整数定数]*y *)
              |_,Int_e v1,_ when v1<0   -> -((-v1)*y)
              (* [負の小数定数]*y *)
              |_,Dbl_e v1,_ when v1<0.0 -> -((-v1)*y)
              (* x*[負の整数定数] *)
              |_,_,Int_e v2 when v2<0   -> -(x*(-v2))
              (* x*[負の小数定数] *)
              |_,_,Dbl_e v2 when v2<0.0 -> -(x*(-v2))
              (* x*(-y) *)
              |_,_,Inv(_,v2) -> -(x*v2)
              (* (-x)*y *)
              |_,Inv(_,v1),_ -> -(v1*y)
              (* [整数定数]*[整数定数] *)
              |(F|C89|C99|NL),Int_e v1,Int_e v2 -> Int_e(v1*v2)
              (* [整数定数]*[小数定数] *)
              |(F|C89|C99|NL),Int_e v1,Dbl_e v2 -> Dbl_e((double v1)*v2)
              (* [小数定数]*[整数定数] *)
              |(F|C89|C99|NL),Dbl_e v1,Int_e v2 -> Dbl_e(v1*(double v2))
              (* [小数定数]*[小数定数] *)
              |(F|C89|C99|NL),Dbl_e v1,Dbl_e v2 -> Dbl_e(v1*v2)
              (* x*(y1+y2+…) or x*(y1-y2) *)
              |_,_,(Add(t,_)|Sub(t,_,_)) -> x*Par(t,y)
              (* (x1+x2+…)*y or (x1-x2)*y *)
              |_,(Add(t,_)|Sub(t,_,_)),_ -> Par(t,x)*y
              (* x*y *)
              |_ -> Mul(num0.ptype(x.etype,y.etype),x,y)
              
        ///<summary>乗算</summary>
        static member (*) (x:num0,y:int) = x*(Int_e y)
              
        ///<summary>乗算</summary>
        static member (*) (x:int,y:num0) = (Int_e x)*y
            
        ///<summary>乗算</summary>
        static member (*) (x:num0,y:double) = x*(Dbl_e y)
        
        ///<summary>乗算</summary>
        static member (*) (x:double,y:num0) = (Dbl_e x)*y
            
        ///<summary>乗算</summary>
        static member (*) ((re:double,im:double),y:num0) =
            (re+num0.uj*(Dbl_e im))*y
        
        ///<summary>乗算</summary>
        static member (*) (x:num0,(re:double,im:double)) =
            x*(re+num0.uj*im)
            
        ///<summary>乗算(xが0でない時だけyを評価し乗算)</summary>
        static member (*) (x:num0,y:unit->num0) =
            match x with
              |Int_e 0 ->
                Int_e 0
              |xx ->
                let yy = y()
                xx*yy
                
        ///<summary>乗算(xの評価が0でない時だけyを評価し乗算)</summary>
        static member (*) (x:unit->num0,y:unit->num0) =
            match x() with
              |Int_e 0 ->
                Int_e 0
              |xx ->
                let yy = y()
                xx*yy
                
        ///<summary>除算</summary>
        static member (/) (x:num0,y:num0) : num0 =
            match x.etype,y.etype with
              |It _,It _ ->
                match (x,y) with
                  (* x/0 *)
                  |_,Int_e 0 ->
                    Console.WriteLine("Error: ゼロ割りを検出しました")
                    Console.Read() |> ignore
                    NaN
                  (* 0/y *)
                  |Int_e 0,_ -> Int_e 0
                  (* x/1 *)
                  |_,Int_e 1 -> x
                  (* [負の整数定数]/[負の整数定数] *)
                  |Int_e v1,Int_e v2 when v1<0 && v2<0 -> Dbl_e((double -v1)/(double -v2))
                  (* [負の整数定数]/[整数定数] *)
                  |Int_e v1,Int_e v2 when v1<0 -> -Dbl_e((double -v1)/(double  v2))
                  (* [整数定数]/[負の整数定数] *)
                  |Int_e v1,Int_e v2 when v2<0 -> -Dbl_e((double  v1)/(double -v2))
                  (* [整数定数]/[整数定数] *)
                  |Int_e v1,Int_e v2 -> Dbl_e((double v1)/(double v2))
                  (* [負の整数定数]/y *)
                  |Int_e v1,_ when v1<0   -> -((-v1)/y)
                  (* x/[負の整数定数] *)
                  |_,Int_e v2 when v2<0   -> -(x/(-v2))
                  (* x/[整数定数] *)
                  |_,Int_e v2             -> x/(double v2)
                  (* (-x)/(-y) *)
                  |Inv(_,v1),Inv(_,v2) -> v1/v2
                  (* x/(-y) *)
                  |_,Inv(_,v2) -> -(x/v2)
                  (* (-x)/y *)
                  |Inv(_,v1),_ -> -(v1/y)
                  (* x/y *)
                  |_ -> x/ToDbl(y)
              |_ ->
                match p.lang,x,y with
                  (* x/0 *)
                  |_,_,Int_e 0 |_,_,Dbl_e 0.0 ->
                    Console.WriteLine("Error: ゼロ割りを検出しました")
                    Console.Read() |> ignore
                    NaN
                  (* 0/y *)
                  |_,Int_e 0,_ -> Int_e 0
                  |_,Dbl_e 0.0,_ -> Dbl_e 0.0
                  |_,_,Dbl_e 1.0 -> x
                  (* [負の整数定数]/[負の整数定数] *)
                  |(F|C89|C99|NL),Dbl_e v1,Int_e v2 when v1<0.0 && v2<0 ->  Dbl_e(( v1)/(double -v2))
                  (* [負の整数定数]/[整数定数] *)
                  |(F|C89|C99|NL),Dbl_e v1,Int_e v2 when v1<0.0         -> -Dbl_e((-v1)/(double v2))
                  (* [整数定数]/[負の整数定数] *)
                  |(F|C89|C99|NL),Dbl_e v1,Int_e v2 when           v2<0 -> -Dbl_e(( v1)/(-(double v2)))
                  (* [整数定数]/[整数定数] *)
                  |(F|C89|C99|NL),Dbl_e v1,Int_e v2                     ->  Dbl_e(v1/(double v2))
                  (* [負の整数定数]/[負の小数定数] *)
                  |(F|C89|C99|NL),Int_e v1,Dbl_e v2 when v1<0 && v2<0.0   ->  Dbl_e((double  v1)/(-v2))
                  (* [負の整数定数]/[小数定数] *)
                  |(F|C89|C99|NL),Int_e v1,Dbl_e v2 when v1<0             -> -Dbl_e((double -v1)/( v2))
                  (* [整数定数]/[負の小数定数] *)
                  |(F|C89|C99|NL),Int_e v1,Dbl_e v2 when         v2<0.0   -> -Dbl_e((double  v1)/(-v2))
                  (* [整数定数]/[小数定数] *)
                  |(F|C89|C99|NL),Int_e v1,Dbl_e v2                       ->  Dbl_e((double v1)/v2)
                  (* [負の小数定数]/[負の小数定数] *)
                  |(F|C89|C99|NL),Dbl_e v1,Dbl_e v2 when v1<0.0 && v2<0.0 ->  Dbl_e((-v1)/(-v2))
                  (* [負の小数定数]/[小数定数] *)
                  |(F|C89|C99|NL),Dbl_e v1,Dbl_e v2 when v1<0.0           -> -Dbl_e((-v1)/( v2))
                  (* [小数定数]/[負の小数定数] *)
                  |(F|C89|C99|NL),Dbl_e v1,Dbl_e v2 when v2<0.0           -> -Dbl_e(( v1)/(-v2))
                  (* [小数定数]/[小数定数] *)
                  |(F|C89|C99|NL),Dbl_e v1,Dbl_e v2                       ->  Dbl_e(v1/v2)
                  (* (-x)/[小数定数] *)
                  |_,Dbl_e v1,_ when v1<0.0 -> -((-v1)/y)
                  (* x/[負の小数定数] *)
                  |_,_,Dbl_e v2 when v2<0.0 -> -(x/(-v2))
                  (* (-x)/[整数定数] *)
                  |_,Int_e v1,_ when v1<0   -> -((-v1)/y)
                  (* x/[負の整数定数] *)
                  |_,_,Int_e v2 when v2<0   -> -(x/(-v2))
                  (* (-x)/(-y) *)
                  |_,Inv(_,v1),Inv(_,v2) -> v1/v2
                  (* x/(-y) *)
                  |_,_,Inv(_,v2) -> -(x/v2)
                  (* (-x)/y *)
                  |_,Inv(_,v1),_ -> -(v1/y)
                  (* x/(y1+y2+…) or x/(y1-y2) or x/(y1*y2) or x/(y1/y2) *)
                  |(F|C89|C99),_,(Add(t,_)|Sub(t,_,_)|Mul(t,_,_)|Div(t,_,_)) -> x/Par(t,y)
                  (* (x1+x2+…)/y or (x1-x2)/y *)
                  |(F|C89|C99),(Add(t,_)|Sub(t,_,_)),_ -> Par(t,x)/y
                  (* x/y *)
                  |_ -> Div(num0.ptype(x.etype,y.etype),x,y)
              
        ///<summary>整数同士の除算(剰余無視)</summary>
        static member (./) (x:num0,y:num0) : num0 =
            match x.etype,y.etype with
              |It _,It _ ->
                match (x,y) with
                  |_,Int_e 0 -> NaN
                  |Int_e 0,_ -> Int_e 0
                  |_,Int_e 1 -> x
                  |Int_e v1,Int_e v2 when v1<0 && v2<0  -> Int_e((-v1)/(-v2))
                  |Int_e v1,_ when v1<0   -> -((-v1)/y)
                  |_,Int_e v2 when v2<0   -> -(x/(-v2))
                  |Int_e v1,Int_e v2 -> Int_e(v1/v2)
                  |Inv(_,v1),Inv(_,v2) -> (v1/v2)
                  |_,Inv(_,v2) -> -(x/v2)
                  |Inv(_,v1),_ -> -(v1/y)
                  |_,(Add(t,_)|Sub(t,_,_)|Mul(t,_,_)|Div(t,_,_)) -> x./Par(t,y)
                  |(Add(t,_)|Sub(t,_,_)),_ -> Par(t,x)./y
                  |_ -> Div(num0.ptype(x.etype,y.etype),x,y)
              |_ ->
                Console.WriteLine("Error: 非整数型変数に対し整数型除算「./」が適用されました")
                Console.Read() |> ignore
                NaN
                
        ///<summary>除算</summary>
        static member (/) (x:num0,y:int) = x/(Int_e y)
        
        ///<summary>除算(小数点以下切り捨て)</summary>
        static member (./) (x:num0,y:int) = x./(Int_e y)

        ///<summary>除算</summary>
        static member (/) (x:int,y:num0) = (Int_e x)/y
            
        ///<summary>除算(小数点以下切り捨て)</summary>
        static member (./) (x:int,y:num0) = (Int_e x)./y

        ///<summary>除算</summary>
        static member (/) (x:num0,y:double) = x/(Dbl_e y)
        
        ///<summary>除算</summary>
        static member (/) (x:double,y:num0) = (Dbl_e x)/y
        
        ///<summary>除算</summary>
        static member (/) ((re:double,im:double),y:num0) =
            (re+num0.uj*(Dbl_e im))/y
            
        ///<summary>除算</summary>
        static member (/) (x:num0,(re:double,im:double)) =
            x/(re+num0.uj*(Dbl_e im))
            
        ///<summary>除算(xが0でない時だけyを評価し除算)</summary>
        static member (/) (x:num0,y:unit->num0) =
            match x with
              |Int_e 0 ->
                Int_e 0
              |xx ->
                let yy = y()
                xx/yy
                
        ///<summary>除算(xの評価が0でない時だけyを評価し除算)</summary>
        static member (/) (x:unit->num0,y:unit->num0) =
            match x() with
              |Int_e 0 ->
                Int_e 0
              |xx ->
                let yy = y()
                xx/yy

        ///<summary>剰余</summary>
        static member (%) (x:num0,y:num0) : num0 =
            match x.etype,y.etype with
              |It _,It _ ->
                match (p.lang,x,y) with
                  |_,_,Int_e 0 -> NaN
                  |_,Int_e 0,_ -> Int_e 0
                  |_,_,Int_e 1 -> Int_e 0
                  |_,Int_e v1,_ when v1<0   -> -((-v1)%y)
                  |_,_,Int_e v2 when v2<0   -> (x%(-v2))
                  |_,Int_e v1,Int_e v2 -> Int_e(v1%v2)
                  |(C89|C99),_,(Add(t,_)|Sub(t,_,_)|Mul(t,_,_)|Div(t,_,_)) -> Mod(x,Par(t,y))
                  |(C89|C99),(Add(t,_)|Sub(t,_,_)),_ -> Mod(Par(t,x),y)
                  |_ -> Mod(x,y)
              |_ ->
                NaN
                
        ///<summary>剰余</summary>
        static member (%) (x:num0,y:int) = x%(Int_e y)
        
        ///<summary>剰余</summary>
        static member (%) (x:int,y:num0) = (Int_e x)%y
        
        ///<summary>実部を取得</summary>
        member this.re
          with get() = 
            match p.lang,this,this.etype with
              |C89,Var(Zt,"uj",[]),_ -> Int_e 0
              |_,_,Zt -> Re(this)
              |_ -> this
              
        ///<summary>虚部を取得</summary>
        member this.im
          with get() = 
            match p.lang,this,this.etype with
              |C89,Var(Zt,"uj",[]),_ -> Int_e 1
              |_,_,Zt -> Im(this)
              |_ -> Int_e 0
              
        ///<summary>式を評価し、Codeに変換</summary>
        member internal this.code
          with get() =
            let its = p.param.ItoS
            let dts = p.param.DtoS
            let (<--) (x:num0) (y:num0) = num0.subst false x y
            match p.lang with
              |F |T ->
                match this with
                  |Int_e(n) -> Code(It 4,its n,[])
                  |Dbl_e(n) -> Code(Dt,dts n,[])
                  |Str_e(s) -> Code(Structure("string"),"\""+s.Replace("\"","\"\"")+"\"",[])
                  |Var(t,n,k) ->
                    //変数名をそのままコードに変換
                    Code(t,n,k)
                  |Inv(t,v) ->
                    //先に中身を評価
                    let (_,u,c) = v.code.str
                    Code(t,"-"+u,c)
                  |Re(v) ->
                    //先に中身を評価
                    let (_,u,c) = v.code.str
                    //real関数で表現
                    Code(Dt,"real("+u+")",c)
                  |Im(v) ->
                    //先に中身を評価
                    let (_,u,c) = v.code.str
                    //aimag関数で表現
                    Code(Dt,"aimag("+u+")",c)
                  |Par(t,v) ->
                    //先に中身を評価
                    let vv = v.code
                    match vv with
                      |Var _ ->
                        //計算結果が変数に代入されているので括弧は不要
                        vv
                      |_ ->
                        let (t,u,c) = vv.str
                        //括弧を追加
                        Code(t,"("+u+")",c)
                  |Add(t,v1) ->
                    //両者の中身を評価
                    let (u1,c1) = 
                        [0..v1.Length-1]
                        |> List.fold (fun (u0,c0) i -> 
                                       match v1.[i] with
                                         |Inv(_,v) ->
                                           let (_,u,c) = v.code.str
                                           u0+(if i=0 then "" else "-")+u,c0@c
                                         |v ->
                                           let (_,u,c) = v.code.str
                                           u0+(if i=0 then "" else "+")+u,c0@c
                                      ) ("",[])
                    //+記号で加算
                    Code(t,u1,c1)
                  |Sub(t,v1,v2) ->
                    //両者の中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //-記号で減算
                    Code(t,u1+"-"+u2,c2@c1)
                  |Mul(t,v1,v2) ->
                    //両者の中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //*記号で加算
                    Code(t,u1+"*"+u2,c2@c1)
                  |Div(t,v1,v2) ->
                    //両者の中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    ///記号で減算
                    Code(t,u1+"/"+u2,c2@c1)
                  |Idx1(t,v,i1) ->
                    //両者の中身を評価
                    let (_,u,c) = v.code.str
                    let (_,u1,c1) = i1.code.str
                    Code(t,u+"("+u1+")",c@c1)
                  |Idx2(t,v,i1,i2) ->
                    //両者の中身を評価
                    let (_,u,c) = v.code.str
                    let (_,u1,c1) = i1.code.str
                    let (_,u2,c2) = i2.code.str
                    Code(t,u+"("+u1+","+u2+")",c@c2@c1)
                  |Idx3(t,v,i1,i2,i3) ->
                    //両者の中身を評価
                    let (_,u,c) = v.code.str
                    let (_,u1,c1) = i1.code.str
                    let (_,u2,c2) = i2.code.str
                    let (_,u3,c3) = i3.code.str
                    Code(t,u+"("+u1+","+u2+","+u3+")",c@c2@c1@c3)
                  |Pow(t,v1,v2) ->
                    //引数を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    Code(t,u1+"**"+u2,c2@c1)
                  |Mod(v1,v2) ->
                    //引数を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    Code(It 4,"mod"+"("+u1+","+u2+")",c2@c1)
                  |Exp(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"exp"+"("+u+")",c)
                  |Sin(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"sin"+"("+u+")",c)
                  |Cos(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"cos"+"("+u+")",c)
                  |Tan(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"tan"+"("+u+")",c)
                  |Asin(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"asin"+"("+u+")",c)
                  |Acos(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"acos"+"("+u+")",c)
                  |Atan(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"atan"+"("+u+")",c)
                  |Atan2(t,v1,v2) ->
                    //引数を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    Code(t,"atan2"+"("+u1+","+u2+")",c2@c1)
                  |Abs(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"abs"+"("+u+")",c)
                  |Log(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"log"+"("+u+")",c)
                  |Log10(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"log10"+"("+u+")",c)
                  |Sqrt(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"sqrt"+"("+u+")",c)
                  |Floor(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"floor"+"("+u+")",c)
                  |Ceiling(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"ceiling"+"("+u+")",c)
                  |ToInt(v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(It 4,"int"+"("+u+")",c)
                  |ToDbl(v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(Dt,"dble"+"("+u+")",c)
                  |Conj(v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(Zt,"conjg"+"("+u+")",c)
                  |Code(a,b,c) ->
                    //変更不要
                    Code(a,b,c)
                  |_ ->
                    NaN
              |C99 ->
                match this with
                  |Int_e(n) -> Code(It 4,its n,[])
                  |Dbl_e(n) -> Code(Dt,dts n,[])
                  |Str_e(s) -> Code(Structure("string"),"\""+s.Replace("\"","\\\"")+"\"",[])
                  |Var(t,n,k) ->
                    //変数名をそのままコードに変換
                    Code(t,n,k)
                  |Inv(t,v) ->
                    //先に中身を評価
                    let (_,u,c) = v.code.str
                    Code(t,"-"+u,c)
                  |Re(v) ->
                    //先に中身を評価
                    let (_,u,c) = v.code.str
                    //real関数で表現
                    Code(Dt,"creal("+u+")",c)
                  |Im(v) ->
                    //先に中身を評価
                    let (_,u,c) = v.code.str
                    //aimag関数で表現
                    Code(Dt,"cimag("+u+")",c)
                  |Par(t,v) ->
                    //先に中身を評価
                    let vv = v.code
                    match vv with
                      |Var _ ->
                        //計算結果が変数に代入されているので括弧は不要
                        vv
                      |_ ->
                        let (_,u,c) = vv.str
                        //括弧を追加
                        Code(t,"("+u+")",c)
                  |Add(t,v1) ->
                    //両者の中身を評価
                    let (u1,c1) = 
                        [0..v1.Length-1]
                        |> List.fold (fun (u0,c0) i -> 
                                       match v1.[i] with
                                         |Inv(_,v) ->
                                           let (_,u,c) = v.code.str
                                           u0+(if i=0 then "" else "-")+u,c0@c
                                         |v ->
                                           let (_,u,c) = v.code.str
                                           u0+(if i=0 then "" else "+")+u,c0@c
                                      ) ("",[])
                    //+記号で加算
                    Code(t,u1,c1)
                  |Sub(t,v1,v2) ->
                    //両者の中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //-記号で減算
                    Code(t,u1+"-"+u2,c2@c1)
                  |Mul(t,v1,v2) ->
                    //両者の中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //*記号で加算
                    Code(t,u1+"*"+u2,c2@c1)
                  |Div(t,v1,v2) ->
                    //両者の中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    ///記号で減算
                    Code(t,u1+"/"+u2,c2@c1)
                  |Idx1(t,v,i1) ->
                    //両者の中身を評価
                    let (_,u,c) = v.code.str
                    let (_,u1,c1) = i1.code.str
                    Code(t,u+"("+u1+")",c@c1)
                  |Idx2(t,v,i1,i2) ->
                    //両者の中身を評価
                    let (_,u,c) = v.code.str
                    let (_,u1,c1) = i1.code.str
                    let (_,u2,c2) = i2.code.str
                    Code(t,u+"("+u1+","+u2+")",c@c2@c1)
                  |Idx3(t,v,i1,i2,i3) ->
                    //両者の中身を評価
                    let (_,u,c) = v.code.str
                    let (_,u1,c1) = i1.code.str
                    let (_,u2,c2) = i2.code.str
                    let (_,u3,c3) = i3.code.str
                    Code(t,u+"("+u1+","+u2+","+u3+")",c@c2@c1@c3)
                  |Pow(t,v1,v2) ->
                    //引数を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    Code(t,"pow("+u1+","+u2+")",c2@c1)
                  |Mod(v1,v2) ->
                    //引数を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    Code(It 4,u1+"%"+u2,c2@c1)
                  |Exp(t,v) ->
                    //引数を評価
                    let (et,u,c) = v.code.str
                    match et with
                      |Zt -> Code(t,"cexp"+"("+u+")",c)
                      |_  -> Code(t,"exp"+"("+u+")",c)
                  |Sin(t,v) ->
                    //引数を評価
                    let (et,u,c) = v.code.str
                    match et with
                      |Zt -> Code(t,"csin"+"("+u+")",c)
                      |_  -> Code(t,"sin"+"("+u+")",c)
                  |Cos(t,v) ->
                    //引数を評価
                    let (et,u,c) = v.code.str
                    match et with
                      |Zt -> Code(t,"ccos"+"("+u+")",c)
                      |_  -> Code(t,"cos"+"("+u+")",c)
                  |Tan(t,v) ->
                    //引数を評価
                    let (et,u,c) = v.code.str
                    match et with
                      |Zt -> Code(t,"ctan"+"("+u+")",c)
                      |_  -> Code(t,"tan"+"("+u+")",c)
                  |Asin(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"asin"+"("+u+")",c)
                  |Acos(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"acos"+"("+u+")",c)
                  |Atan(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"atan"+"("+u+")",c)
                  |Atan2(t,v1,v2) ->
                    //引数を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    Code(t,"atan2"+"("+u1+","+u2+")",c2@c1)
                  |Abs(t,v) ->
                    //引数を評価
                    let (et,u,c) = v.code.str
                    match et with
                      |Zt -> Code(t,"cabs"+"("+u+")",c)
                      |_  -> Code(t,"abs"+"("+u+")",c)
                  |Log(t,v) ->
                    //引数を評価
                    let (et,u,c) = v.code.str
                    match et with
                      |Zt -> Code(t,"clog"+"("+u+")",c)
                      |_  -> Code(t,"log"+"("+u+")",c)
                  |Log10(t,v) ->
                    //引数を評価
                    let (et,u,c) = v.code.str
                    match et with
                      |Zt -> Code(t,"clog10"+"("+u+")",c)
                      |_  -> Code(t,"log10"+"("+u+")",c)
                  |Sqrt(t,v) ->
                    //引数を評価
                    let (et,u,c) = v.code.str
                    match et with
                      |Zt -> Code(t,"csqrt"+"("+u+")",c)
                      |_  -> Code(t,"sqrt"+"("+u+")",c)
                  |Floor(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"floor"+"("+u+")",c)
                  |Ceiling(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"ceiling"+"("+u+")",c)
                  |ToInt(v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(It 4,"(int)"+"("+u+")",c)
                  |ToDbl(v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(Dt,"(double)"+"("+u+")",c)
                  |Conj(v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(Zt,"conj"+"("+u+")",c)
                  |Code(a,b,c) ->
                    //変更不要
                    Code(a,b,c)
                  |_ ->
                    NaN
              |C89 ->
                match this with
                  |Int_e(n) ->
                    Code(It 4,its n,[])
                  |Dbl_e(n) ->
                    Code(Dt,dts n,[])
                  |Inv(Zt,v) ->
                    //複素数の計算なので実部・虚部に分けて計算
                    //両者の中身を評価
                    let v_ = v.code
                    let (_,_,c) = v_.str
                    //一時変数を生成
                    let a:num0 = Var(p.param.getvar(Zt))
                    //加算の結果を一時変数に代入
                    a.re <-- -v_.re
                    a.im <-- -v_.im
                    Var(Zt,a.name,[a]@c)
                  |Inv(t,v) ->
                    //先に中身を評価
                    let (_,u,c) = v.code.str
                    Code(t,"-"+u,c)
                  |Re(Var(_,v,k)) ->
                    //メンバ変数reで表現
                    Code(Dt,v+".r",k)
                  |Re(v) ->
                    //一時変数を生成
                    let a = Var(p.param.getvar(Zt))
                    //一時変数に代入
                    a <-- v
                    //先に中身を評価
                    let (_,u,c) = a.code.str
                    //メンバ変数reで表現
                    Code(Dt,u+".r",a::c)
                  |Im(Var(_,v,k)) ->
                    //メンバ変数iで表現
                    Code(Dt,v+".i",k)
                  |Im(v) ->
                    //一時変数を生成
                    let a = Var(p.param.getvar(Zt))
                    //一時変数に代入
                    a <-- v
                    //先に中身を評価
                    let (_,u,c) = a.code.str
                    //メンバ変数iで表現
                    Code(Dt,u+".i",a::c)
                  |Par(t,v) ->
                    //先に中身を評価
                    let vv = v.code
                    match vv with
                      |Var _ ->
                        //計算結果が変数に代入されているので括弧は不要
                        vv
                      |_ ->
                        let (_,u,c) = vv.str
                        //括弧を追加
                        Code(t,"("+u+")",c)
                  |Add(Zt,v1) ->
                    //複素数の計算なので実部・虚部に分けて計算
                    //両者の中身を評価
                    let (re,im,c1) = 
                        [0..v1.Length-1]
                        |> List.fold (fun (re,im,c0) i -> 
                                       match v1.[i] with
                                         |Inv(_,v) ->
                                           let (t,u,c) = v.code.str
                                           re+(if i=0 then "" else "-")+u+(if t=Zt then ".r" else ""),im+(if i=0 then "" else "-")+u+(if t=Zt then ".i" else ""),c0@c
                                         |v ->
                                           let (t,u,c) = v.code.str
                                           re+(if i=0 then "" else "+")+u+(if t=Zt then ".r" else ""),im+(if t=Zt then (if i=0 then "" else "+")+u+".i" else ""),c0@c) ("","",[])
                    //一時変数を生成
                    let a:num0 = Var(p.param.getvar(Zt))
                    //加算の結果を一時変数に代入
                    p.param.codewrite(a.name + ".r" + " = " + re + ";")
                    p.param.codewrite(a.name + ".i" + " = " + im + ";")
                    Var(Zt,a.name,[a]@c1)
                  |Add(t,v1) ->
                    let (u1,c1) = 
                        [0..v1.Length-1]
                        |> List.fold (fun (u0,c0) i -> 
                                       let (_,u,c) = v1.[i].code.str
                                       u0+(if i=0 then "" else "+")+u,c0@c) ("",[])
                    //+記号で加算
                    Code(t,u1,c1)
                  |Sub(Zt,v1,v2) ->
                    //複素数の計算なので実部・虚部に分けて計算
                    //両者の中身を評価
                    let v1_ = v1.code
                    let v2_ = v2.code
                    let (_,_,c1) = v1_.str
                    let (_,_,c2) = v2_.str
                    //一時変数を生成
                    let a:num0 = Var(p.param.getvar(Zt))
                    //減算の結果を一時変数に代入
                    a.re <-- (v1_.re - v2_.re)
                    a.im <-- (v1_.im - v2_.im)
                    Var(Zt,a.name,[a]@c2@c1)
                  |Sub(t,v1,v2) ->
                    //両者の中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //-記号で減算
                    Code(t,u1+"-"+u2,c2@c1)
                  |Mul(Zt,v1,v2) ->
                    //複素数の計算なので実部・虚部に分けて計算
                    //両者の中身を評価
                    let v1_ = v1.code
                    let v2_ = v2.code
                    let (_,_,c1) = v1_.str
                    let (_,_,c2) = v2_.str
                    //一時変数を生成
                    let a:num0 = Var(p.param.getvar(Zt))
                    //乗算の結果を一時変数に代入
                    a.re <-- (v1_.re * v2_.re - v1_.im * v2_.im)
                    a.im <-- (v1_.re * v2_.im + v1_.im * v2_.re)
                    Var(Zt,a.name,[a]@c2@c1)
                  |Mul(t,v1,v2) ->
                    //両者の中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //*記号で乗算
                    Code(t,u1+"*"+u2,c2@c1)
                  |Div(Zt,v1,v2) when v2.etype=Zt ->
                    //複素数の計算なので実部・虚部に分けて計算
                    //両者の中身を評価
                    let v1_ = v1.code
                    let v2_ = v2.code
                    let (_,_,c1) = v1_.str
                    let (_,_,c2) = v2_.str
                    //一時変数を生成
                    let a:num0 = Var(p.param.getvar(Dt))
                    let b:num0 = Var(p.param.getvar(Zt))
                    //除算の結果を一時変数に代入
                    a <-- v2_.re * v2_.re + v2_.im * v2_.im
                    b.re <-- (v1_.re * v2_.re + v1_.im * v2_.im)/a
                    b.im <-- (v1_.im * v2_.re - v1_.re * v2_.im)/a
                    Var(Zt,b.name,[b;a;]@c2@c1)
                  |Div(Zt,v1,v2) ->
                    //複素数の計算なので実部・虚部に分けて計算
                    //一時変数を生成
                    let b:num0 = Var(p.param.getvar(Zt))
                    //両者の中身を評価
                    let v1_ = v1.code
                    let (_,_,c1) = v1_.str
                    //除算の結果を一時変数に代入
                    b.re <-- v1_.re/v2
                    b.im <-- v1_.im/v2
                    Var(Zt,b.name,[b;]@c1)
                  |Div(t,v1,v2) ->
                    //両者の中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    ///記号で除算
                    Code(t,u1+"/"+u2,c2@c1)
                  |Pow(t,v1,v2) ->
                    //引数を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    Code(t,"pow"+"("+u1+","+u2+")",c2@c1)
                  |Mod(v1,v2) ->
                    //引数を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    Code(It 4,u1+"%"+u2,c2@c1)
                  |Exp(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"exp"+"("+u+")",c)
                  |Sin(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"sin"+"("+u+")",c)
                  |Cos(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"cos"+"("+u+")",c)
                  |Tan(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"tan"+"("+u+")",c)
                  |Asin(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"asin"+"("+u+")",c)
                  |Acos(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"acos"+"("+u+")",c)
                  |Atan(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"atan"+"("+u+")",c)
                  |Atan2(t,v1,v2) ->
                    //引数を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    Code(t,"atan2"+"("+u1+","+u2+")",c2@c1)
                  |Abs(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"abs"+"("+u+")",c)
                  |Log(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"log"+"("+u+")",c)
                  |Log10(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"log10"+"("+u+")",c)
                  |Sqrt(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"sqrt"+"("+u+")",c)
                  |Floor(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"floor"+"("+u+")",c)
                  |Ceiling(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"ceiling"+"("+u+")",c)
                  |ToInt(v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(It 4,"(int)"+"("+u+")",c)
                  |ToDbl(v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(Dt,"(double)"+"("+u+")",c)
                  |Conj(v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(Zt,"conjg"+"("+u+")",c)
                  |Code(a,b,c) ->
                    //変更不要
                    Code(a,b,c)
                  |x -> x
              |H ->
                match this with
                  |Int_e(n) -> Code(It 4,its n,[])
                  |Dbl_e(n) -> Code(Dt,dts n,[])
                  |Str_e(s) -> Code(Structure("string"),s,[])
                  |Var(t,n,k) -> Code(t,n,k)
                  |Inv(t,v) ->
                    //先に中身を評価
                    let (_,u,c) = v.code.str
                    Code(t,"<mo>-</mo>"+u,c)
                  |Re(v) ->
                    //先に中身を評価
                    let (_,u,c) = v.code.str
                    //real関数で表現
                    Code(Dt,"<mi>Re</mi><mo>&af;</mo><mrow><mo>(</mo>"+u+"<mo>)</mo></mrow>",c)
                  |Im(v) ->
                    //先に中身を評価
                    let (_,u,c) = v.code.str
                    //aimag関数で表現
                    Code(Dt,"<mi>Im</mi><mo>&af;</mo><mrow><mo>(</mo>"+u+"<mo>)</mo></mrow>",c)
                  |Par(t,v) ->
                    //先に中身を評価
                    let vv = v.code
                    match vv with
                      |Var _ ->
                        //計算結果が変数に代入されているので括弧は不要
                        vv
                      |_ ->
                        let (_,u,c) = vv.str
                        //括弧を追加
                        Code(t,"<mrow><mo>(</mo>"+u+"<mo>)</mo></mrow>",c)
                  |Add(t,v1) ->
                    //両者の中身を評価
                    let (u1,c1) = 
                        [0..v1.Length-1]
                        |> List.fold (fun (u0,c0) i -> 
                                       match v1.[i] with
                                         |Inv(_,v) ->
                                           let (_,u,c) = v.code.str
                                           u0+(if i=0 then "" else "<mo>-</mo>")+u,c0@c
                                         |v ->
                                           let (_,u,c) = v.code.str
                                           u0+(if i=0 then "" else "<mo>+</mo>")+u,c0@c
                                      ) ("",[])
                    //+記号で加算
                    Code(t,u1,c1)
                  |Sub(t,v1,v2) ->
                    //両者の中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //-記号で減算
                    Code(t,u1+"<mo>-</mo>"+u2,c2@c1)
                  |Mul(t,v1,v2) ->
                    //両者の中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //*記号で加算
                    Code(t,u1+"<mo mathcolor=\"#999\">&middot;</mo>"+u2,c2@c1)
                  |Div(t,v1,v2) ->
                    //両者の中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    ///記号で減算
                    Code(t,"<mstyle displaystyle=\"true\"><mfrac><mrow>"+u1+"</mrow><mrow>"+u2+"</mrow></mfrac></mstyle>",c2@c1)
                  |Idx1(t,v,i1) ->
                    //両者の中身を評価
                    let (_,u,c) = v.code.str
                    let (_,u1,c1) = i1.code.str
                    Code(t,u+"<mo>&af;</mo><mrow><mo>(</mo>"+u1+"<mo>)</mo></mrow>",c@c1)
                  |Idx2(t,v,i1,i2) ->
                    //両者の中身を評価
                    let (_,u,c) = v.code.str
                    let (_,u1,c1) = i1.code.str
                    let (_,u2,c2) = i2.code.str
                    Code(t,u+"<mo>&af;</mo><mrow><mo>(</mo>"+u1+"<mo>,</mo>"+u2+"<mo>)</mo></mrow>",c@c2@c1)
                  |Idx3(t,v,i1,i2,i3) ->
                    //両者の中身を評価
                    let (_,u,c) = v.code.str
                    let (_,u1,c1) = i1.code.str
                    let (_,u2,c2) = i2.code.str
                    let (_,u3,c3) = i3.code.str
                    Code(t,u+"<mo>&af;</mo><mrow><mo>(</mo>"+u1+"<mo>,</mo>"+u2+"<mo>,</mo>"+u3+"<mo>)</mo></mrow>",c@c2@c1@c3)
                  |Pow(t,v1,v2) ->
                    //引数を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    Code(t,"<msup><mrow>"+u1+"</mrow><mrow>"+u2+"</mrow></msup>",c2@c1)
                  |Mod(v1,v2) ->
                    //引数を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    Code(It 4,"<mi>mod</mi>"+"<mo>&af;</mo><mrow><mo>(</mo>"+u1+"<mo>,</mo>"+u2+"<mo>)</mo></mrow>",c2@c1)
                  |Exp(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"<mi>exp</mi>"+"<mo>&af;</mo><mrow><mo>(</mo>"+u+"<mo>)</mo></mrow>",c)
                  |Sin(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"<mi>sin</mi>"+"<mo>&af;</mo><mrow><mo>(</mo>"+u+"<mo>)</mo></mrow>",c)
                  |Cos(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"<mi>cos</mi>"+"<mo>&af;</mo><mrow><mo>(</mo>"+u+"<mo>)</mo></mrow>",c)
                  |Tan(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"<mi>tan</mi>"+"<mo>&af;</mo><mrow><mo>(</mo>"+u+"<mo>)</mo></mrow>",c)
                  |Asin(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"<mi>asin</mi>"+"<mo>&af;</mo><mrow><mo>(</mo>"+u+"<mo>)</mo></mrow>",c)
                  |Acos(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"<mi>acos</mi>"+"<mo>&af;</mo><mrow><mo>(<mo>"+u+"<mo>)<mo></mrow>",c)
                  |Atan(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"<mi>atan</mi>"+"<mo>&af;</mo><mrow><mo>(<mo>"+u+"<mo>)<mo></mrow>",c)
                  |Atan2(t,v1,v2) ->
                    //引数を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    Code(t,"<mi>atan2</mi>"+"<mo>&af;</mo><mrow><mo>(<mo>"+u1+","+u2+"<mo>)<mo></mrow>",c2@c1)
                  |Abs(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"<mi>abs</mi>"+"<mo>&af;</mo><mrow><mo>(</mo>"+u+"<mo>)</mo></mrow>",c)
                  |Log(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"<mi>log</mi>"+"<mo>&af;</mo><mrow><mo>(</mo>"+u+"<mo>)</mo></mrow>",c)
                  |Log10(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"<mi>log10</mi>"+"<mo>&af;</mo><mrow><mo>(</mo>"+u+"<mo>)</mo></mrow>",c)
                  |Sqrt(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"<msqrt>"+"<mrow>"+u+"</mrow></msqrt>",c)
                  |Floor(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"<mi>floor</mi>"+"<mo>&af;</mo><mrow><mo>(</mo>"+u+"<mo>)</mo></mrow>",c)
                  |Ceiling(t,v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(t,"<mi>ceiling</mi>"+"<mo>&af;</mo><mrow><mo>(</mo>"+u+"<mo>)</mo></mrow>",c)
                  |ToInt(v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(It 4,"<mi>int</mi>"+"<mo>&af;</mo><mrow><mo>(</mo>"+u+"<mo>)</mo></mrow>",c)
                  |ToDbl(v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(Dt,"<mi>dble</mi>"+"<mo>&af;</mo><mrow><mo>(</mo>"+u+"<mo>)</mo></mrow>",c)
                  |Conj(v) ->
                    //引数を評価
                    let (_,u,c) = v.code.str
                    Code(Zt,"<mi>conjg</mi>"+"<mo>&af;</mo><mrow><mo>(</mo>"+u+"<mo>)</mo></mrow>",c)
                  |Code(a,b,c) ->
                    //変更不要
                    Code(a,b,c)
                  |_ ->
                    NaN
              |NL ->
                NaN
                
        static member internal type_subst_warning(t1,t2)=
            match t1,t2 with
              |(It _|Dt|Zt),It _ |(Dt|Zt),Dt |Zt,Zt -> ()
              |It _,Dt ->
                p.param.codewrite("Warning: double型変数をint型に代入しました")
                Console.WriteLine("Warning: double型変数をint型に代入しました")
                Console.Read() |> ignore
              |It _,Zt ->
                p.param.codewrite("Warning: complex型変数をint型に代入しました")
                Console.WriteLine("Warning: complex型変数をint型に代入しました")
                Console.Read() |> ignore
              |Dt,Zt ->
                p.param.codewrite("Warning: complex型変数をdouble型に代入しました")
                Console.WriteLine("Warning: complex型変数をdouble型に代入しました")
                Console.Read() |> ignore
              |_ -> 
                p.param.codewrite("Warning: 不明な型の代入です")
                Console.WriteLine("Warning: 不明な型の代入です")
                Console.Read() |> ignore

        ///<summary>代入</summary>
        static member internal subst (dp:bool) (x:num0) (y:num0) =
            let (<--) (x:num0) (y:num0) = num0.subst false x y
            match p.lang with
              |F ->
                let p = p.param
                let (t1,u1,c1) = x.code.str
                let (t2,u2,c2) = y.code.str
                num0.type_subst_warning(t1,t2)
                p.codewrite(u1 + " = " + u2)
                //必要に応じて一時変数を削除
                if dp then
                    c2@c1 |> List.iter (fun c -> match c with |Var(t,n,_) -> p.dispose(t,n) |_ -> ())
              |C89 ->
                let p = p.param
                let xx = x.code
                let yy = y.code
                let (t1,u1,c1) = xx.str
                let (t2,u2,c2) = yy.str
                num0.type_subst_warning(t1,t2)
                match t1,t2 with
                  |Zt,Zt ->
                    xx.re <-- yy.re
                    xx.im <-- yy.im
                  |Zt,_ ->
                    xx.re <-- yy
                    xx.im <-- (Int_e 0)
                  |_ ->
                    p.codewrite(u1 + " = " + u2 + ";")
                //必要に応じて一時変数を削除
                if dp then
                    c2@c1 |> List.iter (fun c -> 
                                         match c with 
                                           |Var(t,n,_) -> 
                                             p.dispose(t,n) 
                                             p.codewrite("// dispose -> " + n)
                                           |_ -> 
                                             ())
              |C99 ->
                let p = p.param
                let (t1,u1,c1) = x.code.str
                let (t2,u2,c2) = y.code.str
                num0.type_subst_warning(t1,t2)
                p.codewrite(u1 + " = " + u2 + ";")
                //必要に応じて一時変数を削除
                if dp then
                    c2@c1 |> List.iter (fun c -> match c with |Var(t,n,_) -> p.dispose(t,n) |_ -> ())
              |T ->
                let p = p.param
                let (t1,u1,c1) = x.code.str
                let (t2,u2,c2) = y.code.str
                num0.type_subst_warning(t1,t2)
                p.codewrite(u1 + " = " + u2)
                //必要に応じて一時変数を削除
                if dp then
                    c2@c1 |> List.iter (fun c -> match c with |Var(t,n,_) -> p.dispose(t,n) |_ -> ())
              |H ->
                let p = p.param
                let (t1,u1,c1) = x.code.str
                let (t2,u2,c2) = y.code.str
                num0.type_subst_warning(t1,t2)
                p.codewrite("<math>\n")
                p.codewrite(u1 + "<mo>&larr;</mo>" + u2)
                p.codewrite("</math>\n")
                p.codewrite("<br/>\n")
                //必要に応じて一時変数を削除
                if dp then
                    c2@c1 |> List.iter (fun c -> match c with |Var(t,n,_) -> p.dispose(t,n) |_ -> ())
              |NL ->
                ()
                    
        ///<summary>yをxに代入</summary>
        static member (<==) (x:num0,y:num0) =
            num0.subst true x y
        ///<summary>yをxに代入</summary>
        static member (<==) (x:num0,y:int) =
            x <== (Int_e y)
        ///<summary>yをxに代入</summary>
        static member (<==) (x:num0,y:double) =
            x <== (Dbl_e y)
        ///<summary>yをxに代入</summary>
        static member (<==) (x:num0,(re:double,im:double)) =
            match p.lang with
              |F|T|C99 ->
                let uj = Var(Zt,"uj",[])
                x <== re+uj*(Dbl_e im)
              |C89 ->
                x.re <== re
                x.im <== im
              |H ->
                let uj = Var(Zt,"&ImaginaryI;",[])
                x <== re+uj*(Dbl_e im)
              |NL ->
                ()
        ///<summary>等号</summary>
        static member (.=) (v1:num0,v2:num0) = Eq(v1,v2)
        ///<summary>等号</summary>
        static member (.=) (v1:int ,v2:num0) = (Int_e v1).=v2
        ///<summary>等号</summary>
        static member (.=) (v1:num0,v2:int ) = v1.=(Int_e v2)
        ///<summary>等号</summary>
        static member (.=) (v1:double,v2:num0) = (Dbl_e v1).=v2
        ///<summary>等号</summary>
        static member (.=) (v1:num0,v2:double) = v1.=(Dbl_e v2)
        
        ///<summary>不等号</summary>
        static member (.=/) (v1:num0,v2:num0) = NEq(v1,v2)
        ///<summary>不等号</summary>
        static member (.=/) (v1:int ,v2:num0) = (Int_e v1).=/v2
        ///<summary>不等号</summary>
        static member (.=/) (v1:num0,v2:int ) = v1.=/(Int_e v2)
        ///<summary>不等号</summary>
        static member (.=/) (v1:double,v2:num0) = (Dbl_e v1).=/v2
        ///<summary>不等号</summary>
        static member (.=/) (v1:num0,v2:double) = v1.=/(Dbl_e v2)
        
        ///<summary>比較（より小）</summary>
        static member (.<) (v1:num0,v2:num0) = Less(v1,v2)
        ///<summary>比較（より小）</summary>
        static member (.<) (v1:int ,v2:num0) = (Int_e v1).<v2
        ///<summary>比較（より小）</summary>
        static member (.<) (v1:num0,v2:int ) = v1.<(Int_e v2)
        ///<summary>比較（より小）</summary>
        static member (.<) (v1:double ,v2:num0) = (Dbl_e v1).<v2
        ///<summary>比較（より小）</summary>
        static member (.<) (v1:num0,v2:double ) = v1.<(Dbl_e v2)
        
        ///<summary>比較（以下）</summary>
        static member (.<=) (v1:num0,v2:num0) = LessEq(v1,v2)
        ///<summary>比較（以下）</summary>
        static member (.<=) (v1:int ,v2:num0) = (Int_e v1).<=v2
        ///<summary>比較（以下）</summary>
        static member (.<=) (v1:num0,v2:int ) = v1.<=(Int_e v2)
        ///<summary>比較（以下）</summary>
        static member (.<=) (v1:double ,v2:num0) = (Dbl_e v1).<=v2
        ///<summary>比較（以下）</summary>
        static member (.<=) (v1:num0,v2:double ) = v1.<=(Dbl_e v2)
        
        ///<summary>比較（より大）</summary>
        static member (.>) (v1:num0,v2:num0) = Greater(v1,v2)
        ///<summary>比較（より大）</summary>
        static member (.>) (v1:int ,v2:num0) = (Int_e v1).>v2
        ///<summary>比較（より大）</summary>
        static member (.>) (v1:num0,v2:int ) = v1.>(Int_e v2)
        ///<summary>比較（より大）</summary>
        static member (.>) (v1:double ,v2:num0) = (Dbl_e v1).>v2
        ///<summary>比較（より大）</summary>
        static member (.>) (v1:num0,v2:double ) = v1.>(Dbl_e v2)
        
        ///<summary>比較（以上）</summary>
        static member (.>=) (v1:num0,v2:num0) = GreaterEq(v1,v2)
        ///<summary>比較（以上）</summary>
        static member (.>=) (v1:int ,v2:num0) = (Int_e v1).>=v2
        ///<summary>比較（以上）</summary>
        static member (.>=) (v1:num0,v2:int ) = v1.>=(Int_e v2)
        ///<summary>比較（以上）</summary>
        static member (.>=) (v1:double ,v2:num0) = (Dbl_e v1).>=v2
        ///<summary>比較（以上）</summary>
        static member (.>=) (v1:num0,v2:double ) = v1.>=(Dbl_e v2)
        
        ///<summary>変数の値を0にリセット</summary>
        member this.clear() =
            this <== 0

        ///<summary>数値をインクリメント</summary>
        member this.increment() =
            match this.etype with
              |It _ -> this <== this + 1
              |_   -> printfn "incrementは整数にのみ適用可能です"
              
        ///<summary>数値をデクリメント</summary>
        member this.decrement() =
            match this.etype with
              |It _ -> this <== this - 1
              |_   -> printfn "incrementは整数にのみ適用可能です"
            
    ///<summary>条件式</summary>
    and bool0 =
        |Eq of num0*num0
        |NEq of num0*num0
        |Greater of num0*num0
        |GreaterEq of num0*num0
        |Less of num0*num0
        |LessEq of num0*num0
        |And of bool0 list
        |Or of bool0 list
        |Null
        
        ///<summary>式を評価し、Codeに変換</summary>
        member internal this.code
          with get() =
            match p.lang with
              |NL ->
                match this with
                  |Eq(v1,v2) ->
                    match v1,v2 with
                      |Int_e v1,Int_e v2 when v1=v2 -> Int_e 1
                      |Int_e v1,Dbl_e v2 when double v1=v2 -> Int_e 1
                      |Dbl_e v1,Int_e v2 when v1=double v2 -> Int_e 1
                      |Dbl_e v1,Dbl_e v2 when v1=v2 -> Int_e 1
                      |Int_e v1,Int_e v2 -> Int_e 0
                      |Int_e v1,Dbl_e v2 -> Int_e 0
                      |Dbl_e v1,Int_e v2 -> Int_e 0
                      |Dbl_e v1,Dbl_e v2 -> Int_e 0
                      |_ -> NaN
                  |NEq(v1,v2) ->
                    match v1,v2 with
                      |Int_e v1,Int_e v2 when v1<>v2 -> Int_e 1
                      |Int_e v1,Dbl_e v2 when double v1<>v2 -> Int_e 1
                      |Dbl_e v1,Int_e v2 when v1<>double v2 -> Int_e 1
                      |Dbl_e v1,Dbl_e v2 when v1<>v2 -> Int_e 1
                      |Int_e v1,Int_e v2 -> Int_e 0
                      |Int_e v1,Dbl_e v2 -> Int_e 0
                      |Dbl_e v1,Int_e v2 -> Int_e 0
                      |Dbl_e v1,Dbl_e v2 -> Int_e 0
                      |_ -> NaN
                  |Greater(v1,v2) ->
                    match v1,v2 with
                      |Int_e v1,Int_e v2 when v1>v2 -> Int_e 1
                      |Int_e v1,Dbl_e v2 when double v1>v2 -> Int_e 1
                      |Dbl_e v1,Int_e v2 when v1>double v2 -> Int_e 1
                      |Dbl_e v1,Dbl_e v2 when v1>v2 -> Int_e 1
                      |Int_e v1,Int_e v2 -> Int_e 0
                      |Int_e v1,Dbl_e v2 -> Int_e 0
                      |Dbl_e v1,Int_e v2 -> Int_e 0
                      |Dbl_e v1,Dbl_e v2 -> Int_e 0
                      |_ -> NaN
                  |GreaterEq(v1,v2) ->
                    match v1,v2 with
                      |Int_e v1,Int_e v2 when v1>=v2 -> Int_e 1
                      |Int_e v1,Dbl_e v2 when double v1>=v2 -> Int_e 1
                      |Dbl_e v1,Int_e v2 when v1>=double v2 -> Int_e 1
                      |Dbl_e v1,Dbl_e v2 when v1>=v2 -> Int_e 1
                      |Int_e v1,Int_e v2 -> Int_e 0
                      |Int_e v1,Dbl_e v2 -> Int_e 0
                      |Dbl_e v1,Int_e v2 -> Int_e 0
                      |Dbl_e v1,Dbl_e v2 -> Int_e 0
                      |_ -> NaN
                  |Less(v1,v2) ->
                    match v1,v2 with
                      |Int_e v1,Int_e v2 when v1<v2 -> Int_e 1
                      |Int_e v1,Dbl_e v2 when double v1<v2 -> Int_e 1
                      |Dbl_e v1,Int_e v2 when v1<double v2 -> Int_e 1
                      |Dbl_e v1,Dbl_e v2 when v1<v2 -> Int_e 1
                      |Int_e v1,Int_e v2 -> Int_e 0
                      |Int_e v1,Dbl_e v2 -> Int_e 0
                      |Dbl_e v1,Int_e v2 -> Int_e 0
                      |Dbl_e v1,Dbl_e v2 -> Int_e 0
                      |_ -> NaN
                  |LessEq(v1,v2) ->
                    match v1,v2 with
                      |Int_e v1,Int_e v2 when v1<=v2 -> Int_e 1
                      |Int_e v1,Dbl_e v2 when double v1<=v2 -> Int_e 1
                      |Dbl_e v1,Int_e v2 when v1<=double v2 -> Int_e 1
                      |Dbl_e v1,Dbl_e v2 when v1<=v2 -> Int_e 1
                      |Int_e v1,Int_e v2 -> Int_e 0
                      |Int_e v1,Dbl_e v2 -> Int_e 0
                      |Dbl_e v1,Int_e v2 -> Int_e 0
                      |Dbl_e v1,Dbl_e v2 -> Int_e 0
                      |_ -> NaN
                  |And(v) ->
                    let u = List.map (fun (x:bool0) -> x.code) v
                    match List.tryFind (fun (x:num0) -> match x with |Int_e 0 |Int_e 1 -> false |_ -> true) u with
                      |Some _ ->
                        NaN
                      |None ->
                        match List.tryFind (fun (x:num0) -> match x with |Int_e 0 -> true |_ -> false) u with
                          |Some _ ->
                            Int_e 0
                          |None ->
                            Int_e 1
                  |Or(v) ->
                    let u = List.map (fun (x:bool0) -> x.code) v
                    match List.tryFind (fun (x:num0) -> match x with |Int_e 0 |Int_e 1 -> false |_ -> true) u with
                      |Some _ ->
                        NaN
                      |None ->
                        match List.tryFind (fun (x:num0) -> match x with |Int_e 1 -> true |_ -> false) u with
                          |Some _ ->
                            Int_e 1
                          |None ->
                            Int_e 0
                  |Null -> NaN
              |F |T ->
                match this with
                  |Eq(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+"=="+u2,c2@c1)
                  |NEq(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+"/="+u2,c2@c1)
                  |Greater(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+">"+u2,c2@c1)
                  |GreaterEq(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+">="+u2,c2@c1)
                  |Less(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+"<"+u2,c2@c1)
                  |LessEq(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+"<="+u2,c2@c1)
                  |And(v) ->
                    //先に中身を評価
                    let uc = v |> List.map (fun q ->
                                   let (_,u1,c1) = q.code.str
                                   (u1,c1))
                    let code = List.fold (fun acc i -> 
                                  let (u,_)=uc.[i]
                                  if i=0 then
                                      acc + "(" + u + ")"
                                  else
                                      acc + " .and. " + "(" + u + ")" ) "" [0..uc.Length-1]
                    let clst = List.fold (fun acc i -> 
                                  let (_,c)=uc.[i]
                                  acc@c) [] [0..uc.Length-1]
                    //コード生成
                    Code(It 4,code,clst)
                  |Or(v) ->
                    //先に中身を評価
                    let uc = v |> List.map (fun q ->
                                   let (_,u1,c1) = q.code.str
                                   (u1,c1))
                    let code = List.fold (fun acc i -> 
                                  let (u,_)=uc.[i]
                                  if i=0 then
                                      acc + "(" + u + ")"
                                  else
                                      acc + " .or. " + "(" + u + ")" ) "" [0..uc.Length-1]
                    let clst = List.fold (fun acc i -> 
                                  let (_,c)=uc.[i]
                                  acc@c) [] [0..uc.Length-1]
                    //コード生成
                    Code(It 4,code,clst)
                  |_ ->
                    NaN
              |C89 |C99 ->
                match this with
                  |Eq(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+"=="+u2,c2@c1)
                  |NEq(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+"!="+u2,c2@c1)
                  |Greater(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+">"+u2,c2@c1)
                  |GreaterEq(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+">="+u2,c2@c1)
                  |Less(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+"<"+u2,c2@c1)
                  |LessEq(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+"<="+u2,c2@c1)
                  |And(v) ->
                    //先に中身を評価
                    let uc = v |> List.map (fun q ->
                                   let (_,u1,c1) = q.code.str
                                   (u1,c1))
                    let code = List.fold (fun acc i -> 
                                  let (u,_)=uc.[i]
                                  if i<>0 then
                                      acc + " && " + "(" + u + ")"
                                  else acc + "(" + u + ")" ) "" [0..uc.Length-1]
                    let clst = List.fold (fun acc i -> 
                                  let (_,c)=uc.[i]
                                  acc@c) [] [0..uc.Length-1]
                    //コード生成
                    Code(It 4,code,clst)
                  |Or(v) ->
                    //先に中身を評価
                    let uc = v |> List.map (fun q ->
                                   let (_,u1,c1) = q.code.str
                                   (u1,c1))
                    let code = List.fold (fun acc i -> 
                                  let (u,_)=uc.[i]
                                  if i<>0 then
                                      acc + " || " + "(" + u + ")"
                                  else acc + "(" + u + ")" ) "" [0..uc.Length-1]
                    let clst = List.fold (fun acc i -> 
                                  let (_,c)=uc.[i]
                                  acc@c) [] [0..uc.Length-1]
                    //コード生成
                    Code(It 4,code,clst)
                  |_ ->
                    num0.NaN
              |H ->
                match this with
                  |Eq(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+"<mo>=</mo>"+u2,c2@c1)
                  |NEq(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+"<mo>&ne;</mo>"+u2,c2@c1)
                  |Greater(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+"<mo>&gt;</mo>"+u2,c2@c1)
                  |GreaterEq(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+"<mo>&ge;</mo>"+u2,c2@c1)
                  |Less(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+"<mo>&lt;</mo>"+u2,c2@c1)
                  |LessEq(v1,v2) ->
                    //先に中身を評価
                    let (_,u1,c1) = v1.code.str
                    let (_,u2,c2) = v2.code.str
                    //real関数で表現
                    Code(It 4,u1+"<mo>&le;</mo>"+u2,c2@c1)
                  |And(v) ->
                    //先に中身を評価
                    let uc = v |> List.map (fun q ->
                                   let (_,u1,c1) = q.code.str
                                   (u1,c1))
                    let code = List.fold (fun acc i -> 
                                  let (u,_)=uc.[i]
                                  if i=0 then
                                      acc + u
                                  else
                                      acc + " <mo>and</mo> " + u) "" [0..uc.Length-1]
                    let clst = List.fold (fun acc i -> 
                                  let (_,c)=uc.[i]
                                  acc@c) [] [0..uc.Length-1]
                    //コード生成
                    Code(It 4,code,clst)
                  |Or(v) ->
                    //先に中身を評価
                    let uc = v |> List.map (fun q ->
                                   let (_,u1,c1) = q.code.str
                                   (u1,c1))
                    let code = List.fold (fun acc i -> 
                                  let (u,_)=uc.[i]
                                  if i=0 then
                                      acc + u
                                  else
                                      acc + " <mo>or</mo> " + u) "" [0..uc.Length-1]
                    let clst = List.fold (fun acc i -> 
                                  let (_,c)=uc.[i]
                                  acc@c) [] [0..uc.Length-1]
                    //コード生成
                    Code(It 4,code,clst)
                  |_ ->
                    NaN
                    
        ///<summary>変数名、式を文字列で取得</summary>
        member internal this.name
          with get() =
            let x = this.code
            match x with
              |Code(_,b,_) -> b
              |_ -> ""

        ///<summary>比較（より小）</summary>
        static member (.<) (v1:bool0,v2:num0) = 
            match v1 with
              |Less(u1,u2) ->
                let x1 = u1.<u2
                let x2 = u2.<v2
                And([x1;x2])
              |LessEq(u1,u2) ->
                let x1 = u1.<=u2
                let x2 = u2.<v2
                And([x1;x2])
              |Greater(u1,u2) ->
                let x1 = u1.>u2
                let x2 = u2.<v2
                And([x1;x2])
              |GreaterEq(u1,u2) ->
                let x1 = u1.>=u2
                let x2 = u2.<v2
                And([x1;x2])
              |And(lst) ->
                let u1 =
                    match lst with
                      |[] -> NaN
                      |_ ->
                        match lst.[lst.Length-1] with
                          |Less(_,u2) -> u2
                          |LessEq(_,u2) -> u2
                          |Greater(_,u2) -> u2
                          |GreaterEq(_,u2) -> u2
                          |_ -> NaN
                let x2 = u1.<v2
                And(lst@[x2])
              |_ -> Null
        ///<summary>比較（より小）</summary>
        static member (.<) (v1:bool0,v2:int ) = v1.<(Int_e v2)
        ///<summary>比較（より小）</summary>
        static member (.<) (v1:bool0,v2:double ) = v1.<(Dbl_e v2)
        
        ///<summary>比較（以下）</summary>
        static member (.<=) (v1:bool0,v2:num0) = 
            match v1 with
              |Less(u1,u2) ->
                let x1 = u1.<u2
                let x2 = u2.<=v2
                And([x1;x2])
              |LessEq(u1,u2) ->
                let x1 = u1.<=u2
                let x2 = u2.<=v2
                And([x1;x2])
              |Greater(u1,u2) ->
                let x1 = u1.>u2
                let x2 = u2.<=v2
                And([x1;x2])
              |GreaterEq(u1,u2) ->
                let x1 = u1.>=u2
                let x2 = u2.<=v2
                And([x1;x2])
              |And(lst) ->
                let u1 =
                    match lst with
                      |[] -> NaN
                      |_ ->
                        match lst.[lst.Length-1] with
                          |Less(_,u2) -> u2
                          |LessEq(_,u2) -> u2
                          |Greater(_,u2) -> u2
                          |GreaterEq(_,u2) -> u2
                          |_ -> NaN
                let x2 = (u1.<=v2)
                And(lst@[x2])
              |_ -> Null
        ///<summary>比較（以下）</summary>
        static member (.<=) (v1:bool0,v2:int ) = v1.<=(Int_e v2)
        ///<summary>比較（以下）</summary>
        static member (.<=) (v1:bool0,v2:double ) = v1.<=(Dbl_e v2)
        
        ///<summary>比較（より大）</summary>
        static member (.>) (v1:bool0,v2:num0) = 
            match v1 with
              |Less(u1,u2) ->
                let x1 = u1.<u2
                let x2 = u2.>v2
                And[x1;x2]
              |LessEq(u1,u2) ->
                let x1 = u1.<=u2
                let x2 = u2.>v2
                And[x1;x2]
              |Greater(u1,u2) ->
                let x1 = u1.>u2
                let x2 = u2.>v2
                And[x1;x2]
              |GreaterEq(u1,u2) ->
                let x1 = u1.>=u2
                let x2 = u2.>v2
                And[x1;x2]
              |And(lst) ->
                let u1 =
                    match lst with
                      |[] -> NaN
                      |_ ->
                        match lst.[lst.Length-1] with
                          |Less(_,u2) -> u2
                          |LessEq(_,u2) -> u2
                          |Greater(_,u2) -> u2
                          |GreaterEq(_,u2) -> u2
                          |_ -> NaN
                let x2 = u1.>v2
                And(lst@[x2])
              |_ -> Null
        ///<summary>比較（より大）</summary>
        static member (.>) (v1:bool0,v2:int ) = v1.>(Int_e v2)
        ///<summary>比較（より大）</summary>
        static member (.>) (v1:bool0,v2:double ) = v1.>(Dbl_e v2)

        ///<summary>比較（以上）</summary>
        static member (.>=) (v1:bool0,v2:num0) = 
            match v1 with
              |Less(u1,u2) ->
                let x1 = u1.<u2
                let x2 = u2.>=v2
                And[x1;x2]
              |LessEq(u1,u2) ->
                let x1 = u1.<=u2
                let x2 = u2.>=v2
                And[x1;x2]
              |Greater(u1,u2) ->
                let x1 = u1.>u2
                let x2 = u2.>=v2
                And[x1;x2]
              |GreaterEq(u1,u2) ->
                let x1 = u1.>=u2
                let x2 = u2.>=v2
                And[x1;x2]
              |And(lst) ->
                let u1 =
                    match lst with
                      |[] -> NaN
                      |_ ->
                        match lst.[lst.Length-1] with
                          |Less(_,u2) -> u2
                          |LessEq(_,u2) -> u2
                          |Greater(_,u2) -> u2
                          |GreaterEq(_,u2) -> u2
                          |_ -> NaN
                let x2 = u1.>=v2
                And(lst@[x2])
              |_ -> Null
        ///<summary>比較（以上）</summary>
        static member (.>=) (v1:bool0,v2:int ) = v1.>=(Int_e v2)
        ///<summary>比較（以上）</summary>
        static member (.>=) (v1:bool0,v2:double ) = v1.>=(Dbl_e v2)
        
