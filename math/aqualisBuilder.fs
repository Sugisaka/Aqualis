namespace Aqualis
    
    type CPSBuilder() =
        member _.Bind(source: (int -> int) -> int, k: int -> int) =
            source k
        member _.Bind(source: (unit -> int) -> int, k: unit -> int) =
            source k
        member _.Return x = x
        
    // 「継続渡し」(CPS) 用の式ビルダー
    // M<'T> = ('T -> 'R) -> 'R という型を想定
    type AqualisBuilder<'R>() =
        
        // let! x = expr のとき、
        //   source は expr の値（CPS: ('T -> 'R) -> 'R）
        //   continuation は x を受け取って次の CPS を返す関数
        member _.Bind (source: ('T -> 'R) -> 'R, continuation: 'T -> (('U -> 'R) -> 'R) ) : ('U -> 'R) -> 'R =
            fun k ->
                // source に「x を受け取ったら continuation x を実行する」継続を渡す
                source (fun x -> continuation x k)
                
        // return v は「v を継続にそのまま渡す」CPS
        member _.Return (value : 'T) : ('T -> 'R) -> 'R =
            fun k -> k value
