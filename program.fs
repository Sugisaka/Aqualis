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
                F
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
                param_lst.[x].cclose()
                param_lst.[x].vclose()
                param_lst.[x].hclose()
                param_lst.[y.[0]].copen()
                param_lst.[y.[0]].vopen()
                param_lst.[y.[0]].hopen()
                param_idx <- y
            |_ -> ()
              
        member __.codewrite(s) = param_lst.[param_idx.[0]].codewrite(s)
        member __.errorID with get() = param_lst.[param_idx.[0]].error_code_counter
        member __.errorIDinc() = param_lst.[param_idx.[0]].error_code_counter_inc()
        member __.debugMode with get() = param_lst.[param_idx.[0]].debugmode
        member __.comment(s) = param_lst.[param_idx.[0]].comment(s)
        member __.indentInc() = param_lst.[param_idx.[0]].indent.inc()
        member __.indentDec() = param_lst.[param_idx.[0]].indent.dec()
        member __.indentSpace = param_lst.[param_idx.[0]].indent.space
        member __.displaySection = param_lst.[param_idx.[0]].displaysection
        member __.i_cache_var = param_lst.[param_idx.[0]].i_cache_var
        member __.d_cache_var = param_lst.[param_idx.[0]].d_cache_var
        member __.z_cache_var = param_lst.[param_idx.[0]].z_cache_var
        member __.i1_cache_var = param_lst.[param_idx.[0]].i1_cache_var
        member __.d1_cache_var = param_lst.[param_idx.[0]].d1_cache_var
        member __.z1_cache_var = param_lst.[param_idx.[0]].z1_cache_var
        member __.i2_cache_var = param_lst.[param_idx.[0]].i2_cache_var
        member __.d2_cache_var = param_lst.[param_idx.[0]].d2_cache_var
        member __.z2_cache_var = param_lst.[param_idx.[0]].z2_cache_var
        member __.i3_cache_var = param_lst.[param_idx.[0]].i3_cache_var
        member __.d3_cache_var = param_lst.[param_idx.[0]].d3_cache_var
        member __.z3_cache_var = param_lst.[param_idx.[0]].z3_cache_var
        member __.ItoS(n) = param_lst.[param_idx.[0]].ItoS(n)
        member __.DtoS(n) = param_lst.[param_idx.[0]].DtoS(n)
        member __.iFormat = param_lst.[param_idx.[0]].int_string_format
        member __.dFormat = param_lst.[param_idx.[0]].double_string_format
        member __.var = param_lst.[param_idx.[0]].var
        
    module Aqualis_base =
        
        ///<summary>trueのとき数式を最適化</summary>
        let mutable isEqSimplify = true
        
        let p = program()
        