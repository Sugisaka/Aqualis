// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
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
                |HTMLSequenceDiagram -> expr.substHS x y c
                |Numeric -> ()
                
            static member equiv (x:expr) (y:expr) (c:program) =
                match c.language with
                |Fortran -> expr.equivF x y c
                |C99 -> expr.equivC x y c
                |Python -> expr.equivPy x y c
                |JavaScript -> expr.equivJ x y c
                |PHP -> expr.equivPh x y c
                |LaTeX -> expr.equivL x y c
                |HTML -> expr.equivH x y c
                |HTMLSequenceDiagram -> expr.equivHS x y c
                |Numeric -> ()
                
            static member equivAlign (x:expr) (y:expr) (c:program) =
                match c.language with
                |Fortran -> expr.equivAlignF x y c
                |C99 -> expr.equivAlignC x y c
                |Python -> expr.equivAlignPy x y c
                |JavaScript -> expr.equivAlignJ x y c
                |PHP -> expr.equivAlignPh x y c
                |LaTeX -> expr.equivAlignL x y c
                |HTML -> expr.equivAlignH x y c
                |HTMLSequenceDiagram -> expr.equivAlignHS x y c
                |Numeric -> ()
                
            static member forLoop (context:GenerationContext) (n1:expr,n2:expr) code =
                let c = context.CurrentProgram
                match c.language with
                |Fortran -> expr.forLoopF context (n1,n2) code
                |C99 -> expr.forLoopC context (n1,n2) code
                |Python -> expr.forLoopPy context (n1,n2) code
                |JavaScript -> expr.forLoopJ context (n1,n2) code
                |PHP -> expr.forLoopPh context (n1,n2) code
                |LaTeX -> expr.forLoopL context (n1,n2) code
                |HTML -> expr.forLoopH context (n1,n2) code
                |HTMLSequenceDiagram -> expr.forLoopHS context (n1,n2) code
                |Numeric -> ()
                
            ///<summary>無限ループ</summary>
            static member loop (context:GenerationContext) code =
                let c = context.CurrentProgram
                match c.language with
                |Fortran -> expr.loopF context code
                |C99 -> expr.loopC context code
                |Python -> expr.loopPy context code
                |JavaScript-> expr.loopJ context code
                |PHP -> expr.loopPh context code
                |LaTeX -> expr.loopL context code
                |HTML -> expr.loopH context code
                |HTMLSequenceDiagram -> expr.loopHS context code
                |Numeric -> ()
                
            ///<summary>条件を満たす間ループ</summary>
            static member whiledo (context:GenerationContext) (cond:expr) = fun code ->
                let c = context.CurrentProgram
                match c.language with
                |Fortran -> expr.whiledoF context cond code
                |C99 -> expr.whiledoC context cond code
                |Python -> expr.whiledoPy context cond code
                |JavaScript -> expr.whiledoJ context cond code
                |PHP -> expr.whiledoPh context cond code
                |LaTeX -> expr.whiledoL context cond code
                |HTML -> expr.whiledoH context cond code
                |HTMLSequenceDiagram -> expr.whiledoHS context cond code
                |Numeric -> ()
                
            ///<summary>指定した範囲でループ</summary>
            static member range (context:GenerationContext) (counter:option<string>) (i1:expr) = fun (i2:expr) -> fun code ->
                let c = context.CurrentProgram
                match c.language with
                |Fortran -> expr.rangeF context counter i1 i2 code
                |C99 -> expr.rangeC context counter i1 i2 code
                |Python -> expr.rangePy context counter i1 i2 code
                |JavaScript -> expr.rangeJ context counter i1 i2 code
                |PHP -> expr.rangePh context counter i1 i2 code
                |LaTeX -> expr.rangeL context counter i1 i2 code
                |HTML -> expr.rangeH context counter i1 i2 code
                |HTMLSequenceDiagram -> expr.rangeHS context counter i1 i2 code
                |Numeric -> expr.rangeN i1 i2 code
                
            ///<summary>指定した範囲でループ(途中脱出可)</summary>
            static member range_exit (context:GenerationContext) (counter:option<string>) (i1:expr) = fun (i2:expr) -> fun code ->
                let c = context.CurrentProgram
                match c.language with
                |Fortran -> expr.range_exitF context counter i1 i2 code
                |C99 -> expr.range_exitC context counter i1 i2 code
                |Python -> expr.range_exitPy context counter i1 i2 code
                |JavaScript -> expr.range_exitJ context counter i1 i2 code
                |PHP -> expr.range_exitPh context counter i1 i2 code
                |LaTeX -> expr.range_exitL context counter i1 i2 code
                |HTML -> expr.range_exitH context counter i1 i2 code
                |HTMLSequenceDiagram -> expr.range_exitHS context counter i1 i2 code
                |Numeric -> ()
                
            static member branch (context:GenerationContext) code =
                let c = context.CurrentProgram
                match c.language with
                |Fortran -> expr.branchF context code
                |C99 -> expr.branchC context code
                |Python -> expr.branchPy context code
                |JavaScript -> expr.branchJ context code
                |PHP -> expr.branchPh context code
                |LaTeX -> expr.branchL context code
                |HTML -> expr.branchH context code
                |HTMLSequenceDiagram -> expr.branchHS context code
                |Numeric -> ()
                
            member this.eval (c:program) =
                match c.language with
                |Fortran -> this.evalF c
                |C99 -> this.evalC c
                |Python -> this.evalPy c
                |JavaScript -> this.evalJ c
                |PHP -> this.evalPh c
                |LaTeX -> this.evalL c
                |HTML -> this.evalH c
                |HTMLSequenceDiagram -> this.evalHS c
                |Numeric -> ""
