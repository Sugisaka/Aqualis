namespace Aqualis
    
    ///<summary>言語を指定</summary>
    type Language =
        ///<summary>Fortran</summary>
        |Fortran
        ///<summary>C</summary>
        |C99
        ///<summary>LaTeX</summary>
        |LaTeX
        ///<summary>HTML</summary>
        |HTML
        ///<summary>Python</summary>
        |Python
        ///<summary>JavaScript</summary>
        |JavaScript
        ///<summary>PHP</summary>
        |PHP
        ///<summary>直接計算</summary>
        |Numeric

    ///<summary>設定のONまたはOFFを指定</summary>
    type Switch =
        |ON
        |OFF

    ///<summary>変数、配列とその次元の指定</summary>
    type VarType =
        ///<summary>変数</summary>
        |A0
        ///<summary>1次元配列(要素数)</summary>
        |A1 of int
        ///<summary>2次元配列(要素数1,要素数2)</summary>
        |A2 of int*int
        ///<summary>3次元配列(要素数1,要素数2,要素数3)</summary>
        |A3 of int*int*int
