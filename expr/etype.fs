namespace Aqualis
    
    ///<summary>変数の型を指定</summary>
    type Etype =
        ///<summary>論理型</summary>
        |Bt
        ///<summary>整数型(バイト数)</summary>
        |It of int
        ///<summary>倍精度浮動小数点型</summary>
        |Dt
        ///<summary>複素数（倍精度）</summary>
        |Zt
        ///<summary>非数値</summary>
        |Nt
        ///<summary>構造体</summary>
        |Structure of string
        
        ///<summary>優先度の高い型を選択</summary>
        static member ( %% ) (x:Etype,y:Etype) = 
            match x,y with
            |Zt,(Zt|Dt|It _) -> Zt
            |(Zt|Dt|It _),Zt -> Zt
            |Dt,(Dt|It _) -> Dt
            |(Dt|It _),Dt -> Dt
            |It a,It b -> It (if a>b then a else b)
            |_ -> Nt
            
        ///<summary>言語設定に従って型名を生成</summary>
        member this.tostring lang = 
            match lang with
            |Fortran ->
                match this with 
                |It 1 -> "integer(1)" 
                |It _ -> "integer" 
                |Dt -> "double precision" 
                |Zt -> "complex(kind(0d0))" 
                |Structure "string" -> "character(100)" 
                |Structure "integer(1)" -> "integer(1)" 
                |Structure "file" -> "integer"
                |Structure sname -> "type("+sname+")"
                |_ -> ""
            |C99 ->
                match this with 
                |It 1 -> "unsigned char" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "double complex"
                |Structure "string" -> "string" 
                |Structure "char" -> "char" 
                |Structure "file" -> "FILE*" 
                |Structure sname -> sname 
                |_ -> ""
            |LaTeX ->
                match this with 
                |It 1 -> "byte" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "complex"
                |Structure "string" -> "char" 
                |Structure "char" -> "char" 
                |Structure sname -> sname 
                |_ -> ""
            |HTML ->
                match this with 
                |It 1 -> "byte" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "complex"
                |Structure "string" -> "char" 
                |Structure "char" -> "char" 
                |Structure sname -> sname 
                |_ -> ""
            |HTMLSequenceDiagram ->
                match this with 
                |It 1 -> "byte" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "complex"
                |Structure "string" -> "char" 
                |Structure "char" -> "char" 
                |Structure sname -> sname 
                |_ -> ""
            |Python ->
                match this with 
                |It 1 -> "int" 
                |It _ -> "int" 
                |Dt -> "float" 
                |Zt -> "complex"
                |Structure "string" -> "str" 
                |Structure "char" -> "str" 
                |Structure "file" -> "io.TextIOWrapper"
                |Structure sname -> sname
                |_ -> ""
            |JavaScript ->
                match this with 
                |It 1 -> "unsigned char" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "double complex"
                |Structure "string" -> "string" 
                |Structure "char" -> "char" 
                |Structure "file" -> "FILE*" 
                |Structure sname -> sname 
                |_ -> ""
            |PHP ->
                match this with 
                |It 1 -> "unsigned char" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "double complex"
                |Structure "string" -> "string" 
                |Structure "char" -> "char" 
                |Structure "file" -> "FILE*" 
                |Structure sname -> sname 
                |_ -> ""
            |Numeric ->
                match this with 
                |It 1 -> "byte" 
                |It _ -> "int" 
                |Dt -> "double" 
                |Zt -> "complex"
                |Structure "string" -> "char" 
                |Structure "char" -> "char" 
                |Structure sname -> sname 
                |_ -> ""
