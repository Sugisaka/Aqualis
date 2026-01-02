namespace Aqualis
    
    [<AutoOpen>]
    module print_ax =
        ///<summary>画面表示</summary>
        type print with
            
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:num1) = 
                iter.num s.size1 <| fun i -> 
                    print.cc i s[i]
                    
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:num2) = 
                iter.num s.size1 <| fun i -> 
                    iter.num s.size2 <| fun j -> 
                        print.ccc i j s[i,j]
                        
            ///<summary>1個の項目を画面表示</summary>
            static member c (s:num3) = 
                iter.num s.size1 <| fun i -> 
                    iter.num s.size2 <| fun j -> 
                        iter.num s.size3 <| fun k -> 
                            print.cccc i j k s[i,j,k]
                            