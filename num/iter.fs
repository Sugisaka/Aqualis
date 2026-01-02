namespace Aqualis
    
    ///<summary>反復処理</summary>
    type iter () =
        
        ///<summary>無限ループ</summary>
        static member loop code = expr.loop programList[prIndex] (fun (ex,i:expr) -> code(ex,num0 i))
                
        ///<summary>条件を満たす間ループ</summary>
        static member whiledo (cond:bool0) = fun code -> expr.whiledo programList[prIndex] cond.Expr code
                
        ///<summary>指定した範囲でループ</summary>
        static member range (i1:num0) = fun (i2:num0) -> fun code -> expr.range programList[prIndex] i1.Expr i2.Expr (fun (i:expr) -> code (num0 i))
                
        ///<summary>指定した範囲でループ(途中脱出可)</summary>
        static member range_exit (i1:num0) = fun (i2:num0) -> fun code -> expr.range_exit programList[prIndex] i1.Expr i2.Expr (fun (ex,i:expr) -> code (ex,num0 i))
                
        ///<summary>指定した範囲でループ</summary>
        static member range (i1:num0,i2:num0) = fun code -> iter.range i1 i2 code
        static member range (i1:num0,i2:int) = fun code -> iter.range (i1,I i2) code
        static member range (i1:int,i2:num0) = fun code -> iter.range (I i1,i2) code
        static member range (i1:int,i2:int) = fun code -> iter.range (I i1,I i2) code
        
        ///<summary>指定した範囲でループ</summary>
        static member range_exit (i1:num0,i2:num0) = fun code -> iter.range_exit i1 i2 code
        static member range_exit (i1:num0,i2:int) = fun code -> iter.range_exit (i1,I i2) code
        static member range_exit (i1:int,i2:num0) = fun code -> iter.range_exit (I i1,i2) code
        static member range_exit (i1:int,i2:int) = fun code -> iter.range_exit (I i1,I i2) code

        ///<summary>0から指定した回数ループ</summary>
        static member num (n1:num0) = fun code -> 
            iter.range _0 (n1-1) code
            
        ///<summary>0から指定した回数ループ</summary>
        static member num (n1:num0,n2:num0) = fun code ->
            iter.num n1 <| fun i ->
                iter.num n2 <| fun j ->
                    code(i,j)
                    
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:num0,n2:num0,n3:num0) = fun code ->
            iter.num n1 <| fun i ->
                iter.num n2 <| fun j ->
                    iter.num n3 <| fun k ->
                        code(i,j,k)
                        
        ///<summary>0から指定した回数ループ</summary>
        static member num (n1:int) = fun code -> 
            iter.num (I n1) code
            
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:int,n2:int) = fun code -> 
            iter.num (I n1,I n2) code
            
        ///<summary>0から指定した回数ループ</summary>
        static member num (n1:int,n2:int,n3:int) = fun code -> 
            iter.num (I n1,I n2,I n3) code
            
        ///<summary>0から指定した回数ループ(途中脱出可)</summary>
        static member num_exit (n1:num0) = fun code -> 
            iter.range_exit _0 (n1-1) code 
            
        ///<summary>lstの各要素に対しcodeを実行</summary>
        static member list (lst:seq<'a>) (code:'a->unit) =
            for a in lst do
                code a
                
    ///<summary>反復処理（処理スキップ）</summary>
    type dummy_iter () =
        
        ///<summary>無限ループ</summary>
        static member loop code = ()
        
        ///<summary>条件を満たす間ループ</summary>
        static member whiledo (l:bool0) = fun code -> ()
        
        ///<summary>指定した範囲でループ</summary>
        static member range (i1:num0) = fun (i2:num0) -> fun code -> ()
        
        ///<summary>指定した範囲でループ(途中脱出可)</summary>
        static member range_exit (i1:num0) = fun (i2:num0) -> fun code -> ()
        
        ///<summary>指定した範囲i=i1->i2,j=j1->j2に対しcode(i,j)を実行</summary>
        static member range (i1:num0,j1:num0) = fun (i2:num0,j2:num0) -> fun code -> ()
        
        ///<summary>指定した範囲i=i1->i2,j=j1->j2に対しcode(i,j)を実行</summary>
        static member range (i1:num0,j1:num0,k1:num0) = fun (i2:num0,j2:num0,k2:num0) -> fun code -> ()
        
        ///<summary>指定した範囲でループ</summary>
        static member range (i1:int) = fun (i2:int) -> fun code -> ()
        
        ///<summary>指定した範囲i=i1->i2,j=j1->j2に対しcode(i,j)を実行</summary>
        static member range (i1:int,j1:int) = fun (i2:int,j2:int) -> fun code -> ()
        
        ///<summary>指定した範囲i=i1->i2,j=j1->j2に対しcode(i,j)を実行</summary>
        static member range (i1:int,j1:int,k1:int) = fun (i2:int,j2:int,k2:int) -> fun code -> ()
        
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:num0) = fun code -> ()
        
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:num0,n2:num0) = fun code -> ()
        
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:num0,n2:num0,n3:num0) = fun code -> ()
        
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:int) = fun code -> ()
        
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:int,n2:int) = fun code -> ()
        
        ///<summary>1から指定した回数ループ</summary>
        static member num (n1:int,n2:int,n3:int) = fun code -> ()
        
        ///<summary>1から指定した回数ループ(途中脱出可)</summary>
        static member num_exit (n1:num0) = fun code -> ()
        
        ///<summary>lstの各要素に対しcodeを実行</summary>
        static member list (lst:seq<'a>) (code:'a->unit) = ()