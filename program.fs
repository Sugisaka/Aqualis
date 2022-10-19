(*
Copyright (c) 2022 Jun-ichiro Sugisaka

This software is released under the MIT License.
http://opensource.org/licenses/mit-license.php
*)
namespace Aqualis
    
    open System
    open System.IO
    open System.Text

    ///<summary>複数のプログラム・関数の管理</summary>
    type program () =
        let mutable param_lst:param list = []
        
        let mutable param_idx = []
    
        ///<summary>作成済みのすべての関数を削除</summary>
        member __.clear() =
            param_lst <- []
            param_idx <- []
            
        ///<summary>メイン関数</summary>
        member __.param_main with get() = param_lst.[0]
        
        ///<summary>インデックスiの関数</summary>
        member __.parami(i:int) = param_lst.[i]
        
        ///<summary>現在生成中の関数</summary>
        member __.param with get() = param_lst.[param_idx.[0]]
        
        ///<summary>現在指定されている言語</summary>
        member __.lang with get() = 
            if param_idx.Length=0 then
              NL
            else
              let pp = param_lst.[param_idx.[0]]
              pp.lang

        ///<summary>関数を追加</summary>
        member __.param_add(lang_,dir_,proj_) =
            //書き込み中の関数があればストリームを閉じる
            if param_idx.Length>0 then
                param_lst.[param_idx.[0]].cclose()
                param_lst.[param_idx.[0]].vclose()
                param_lst.[param_idx.[0]].hclose()
            let p = new param(lang_,dir_,proj_)
            param_lst <- param_lst@[p]
            param_idx <- (param_lst.Length-1)::param_idx
    
        ///<summary>呼び出し元の関数に戻る</summary>
        member __.param_back() =
            match param_idx with
              |x::y ->
                param_lst.[y.[0]].copen()
                param_lst.[y.[0]].vopen()
                param_lst.[y.[0]].hopen()
                param_idx <- y
              |_ -> ()
              
    module Aqualis_base =
        
        let p = program()
        