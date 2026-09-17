// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis
    
    /// Computation expression builder for continuation-passing computations
    /// represented as <c>('T -> 'R) -> 'R</c>.
    type AqualisBuilder<'R>() =
        
        // let! x = expr のとき、
        //   source は expr の値（CPS: ('T -> 'R) -> 'R）
        //   continuation は x を受け取って次の CPS を返す関数
        /// Runs the source continuation, then passes its value to the next computation.
        member _.Bind (source: ('T -> 'R) -> 'R, continuation: 'T -> (('U -> 'R) -> 'R) ) : ('U -> 'R) -> 'R =
            fun k ->
                // source に「x を受け取ったら continuation x を実行する」継続を渡す
                source (fun x -> continuation x k)
                
        // return v は「v を継続にそのまま渡す」CPS
        /// Lifts a value into a continuation-passing computation.
        member _.Return (value : 'T) : ('T -> 'R) -> 'R =
            fun k -> k value
