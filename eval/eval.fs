namespace Aqualis
    
    [<AutoOpen>]
    module exprEval =
        
        type expr with
            
            static member subst (x:expr) (y:expr) (c:program) =
                match c.language with
                |Fortran -> expr.substF x y c
                |C99 -> expr.substC x y c
                |Python -> expr.substPy x y c
                |JavaScript -> expr.substJ x y c
                |PHP -> expr.substPh x y c
                |LaTeX -> expr.substL x y c
                |HTML -> expr.substH x y c
                |_ -> expr.substF x y c
                
            static member equiv (x:expr) (y:expr) (c:program) =
                match c.language with
                |Fortran -> expr.equivF x y c
                |C99 -> expr.equivC x y c
                |Python -> expr.equivPy x y c
                |JavaScript -> expr.equivJ x y c
                |PHP -> expr.equivPh x y c
                |LaTeX -> expr.equivL x y c
                |HTML -> expr.equivH x y c
                |_ -> expr.equivF x y c
                
            static member equivAlign (x:expr) (y:expr) (c:program) =
                match c.language with
                |Fortran -> expr.equivAlignF x y c
                |C99 -> expr.equivAlignC x y c
                |Python -> expr.equivAlignPy x y c
                |JavaScript -> expr.equivAlignJ x y c
                |PHP -> expr.equivAlignPh x y c
                |LaTeX -> expr.equivAlignL x y c
                |HTML -> expr.equivAlignH x y c
                |_ -> expr.equivAlignF x y c
                
            static member forLoop (c:program) (n1:expr,n2:expr) code =
                match c.language with
                |Fortran -> expr.forLoopF c (n1,n2) code
                |C99 -> expr.forLoopC c (n1,n2) code
                |Python -> expr.forLoopPy c (n1,n2) code
                |JavaScript -> expr.forLoopJ c (n1,n2) code
                |PHP -> expr.forLoopPh c (n1,n2) code
                |LaTeX -> expr.forLoopL c (n1,n2) code
                |HTML -> expr.forLoopH c (n1,n2) code
                |_ -> expr.forLoopF c (n1,n2) code
                
            ///<summary>無限ループ</summary>
            static member loop (c:program) code =
                match c.language with
                |Fortran -> expr.loopF c code
                |C99 -> expr.loopC c code
                |Python -> expr.loopPy c code
                |JavaScript-> expr.loopJ c code
                |PHP -> expr.loopPh c code
                |LaTeX -> expr.loopL c code
                |HTML -> expr.loopH c code
                |_ -> expr.loopF c code
                
            ///<summary>条件を満たす間ループ</summary>
            static member whiledo (c:program) (cond:expr) = fun code ->
                match c.language with
                |Fortran -> expr.whiledoF c cond code
                |C99 -> expr.whiledoC c cond code
                |Python -> expr.whiledoPy c cond code
                |JavaScript -> expr.whiledoJ c cond code
                |PHP -> expr.whiledoPh c cond code
                |LaTeX -> expr.whiledoL c cond code
                |HTML -> expr.whiledoH c cond code
                |_ -> expr.whiledoF c cond code
                
            ///<summary>指定した範囲でループ</summary>
            static member range (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match c.language with
                |Fortran -> expr.rangeF c i1 i2 code
                |C99 -> expr.rangeC c i1 i2 code
                |Python -> expr.rangePy c i1 i2 code
                |JavaScript -> expr.rangeJ c i1 i2 code
                |PHP -> expr.rangePh c i1 i2 code
                |LaTeX -> expr.rangeL c i1 i2 code
                |HTML -> expr.rangeH c i1 i2 code
                |_ -> expr.rangeF c i1 i2 code
                
            ///<summary>指定した範囲でループ(途中脱出可)</summary>
            static member range_exit (c:program) (i1:expr) = fun (i2:expr) -> fun code -> 
                match c.language with
                |Fortran -> expr.range_exitF c i1 i2 code
                |C99 -> expr.range_exitC c i1 i2 code
                |Python -> expr.range_exitPy c i1 i2 code
                |JavaScript -> expr.range_exitJ c i1 i2 code
                |PHP -> expr.range_exitPh c i1 i2 code
                |LaTeX -> expr.range_exitL c i1 i2 code
                |HTML -> expr.range_exitH c i1 i2 code
                |_ -> expr.range_exitF c i1 i2 code
                
            static member branch (c:program) code =
                match c.language with
                |Fortran -> expr.branchF c code
                |C99 -> expr.branchC c code
                |Python -> expr.branchPy c code
                |JavaScript -> expr.branchJ c code
                |PHP -> expr.branchPh c code
                |LaTeX -> expr.branchL c code
                |HTML -> expr.branchH c code
                |_ -> expr.branchF c code
                
            member this.eval (c:program) =
                match c.language with
                |Fortran -> this.evalF c
                |C99 -> this.evalC c
                |Python -> this.evalPy c
                |JavaScript -> this.evalJ c
                |PHP -> this.evalPh c
                |LaTeX -> this.evalL c
                |HTML -> this.evalH c
                |_ -> this.evalF c
