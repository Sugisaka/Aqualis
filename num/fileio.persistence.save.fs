//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    [<AutoOpen>]
    module ContextIoSaveExtensions =
        type ContextIo with
            ///<summary>配列をファイルに保存</summary>
            member this.save_text (f:int3,filename:exprString) =
                this.fileOutput filename <| fun w ->
                    f.foreach <| fun (i,j,k) ->
                        w.tt <| i++j++k++f[i,j,k]
    
            ///<summary>配列をファイルに保存</summary>
            member this.save_text (f:int2,filename:exprString) =
                this.fileOutput filename <| fun w ->
                    f.foreach <| fun (i,j) ->
                        w.tt <| i++j++f[i,j]
    
            ///<summary>配列をファイルに保存</summary>
            member this.save_text (f:int1,filename:exprString) =
                this.fileOutput filename <| fun w ->
                    f.foreach <| fun i ->
                        w.tt <| i++f[i]
    
            ///<summary>数値をファイルに保存</summary>
            member this.save_text (f:int0,filename:exprString) =
                this.fileOutput filename <| fun w ->
                    w.t f
    
            ///<summary>配列をファイルに保存</summary>
            member this.save_text (f:double3,filename:exprString) =
                this.fileOutput filename <| fun w ->
                    f.foreach <| fun (i,j,k) ->
                        w.tt <| i++j++k++f[i,j,k]
    
            ///<summary>配列をファイルに保存</summary>
            member this.save_text (f:double2,filename:exprString) =
                this.fileOutput filename <| fun w ->
                    f.foreach <| fun (i,j) ->
                        w.tt <| i++j++f[i,j]
    
            ///<summary>配列をファイルに保存</summary>
            member this.save_text (f:double1,filename:exprString) =
                this.fileOutput filename <| fun w ->
                    f.foreach <| fun i ->
                        w.tt <| i++f[i]
    
            ///<summary>数値をファイルに保存</summary>
            member this.save_text (f:double0,filename:exprString) =
                this.fileOutput filename <| fun w ->
                    w.t f
    
            ///<summary>配列をファイルに保存</summary>
            member this.save_text (f:complex3,filename:exprString) =
                this.fileOutput filename <| fun w ->
                    f.foreach <| fun (i,j,k) ->
                        w.tt <| i++j++k++f[i,j,k]
    
            ///<summary>配列をファイルに保存</summary>
            member this.save_text (f:complex2,filename:exprString) =
                this.fileOutput filename <| fun w ->
                    f.foreach <| fun (i,j) ->
                        w.tt <| i++j++f[i,j]
    
            ///<summary>配列をファイルに保存</summary>
            member this.save_text (f:complex1,filename:exprString) =
                this.fileOutput filename <| fun w ->
                    f.foreach <| fun i ->
                        w.tt <| i++f[i]
    
            ///<summary>数値をファイルに保存</summary>
            member this.save_text (f:complex0,filename:exprString) =
                this.fileOutput filename <| fun w ->
                    w.t f
    
            ///<summary>配列をファイルに保存</summary>
            member this.save_text (f:int3,filename:string) = this.save_text(f,st filename)
            member this.save_text (f:int2,filename:string) = this.save_text(f,st filename)
            member this.save_text (f:int1,filename:string) = this.save_text(f,st filename)
            member this.save_text (f:int0,filename:string) = this.save_text(f,st filename)
            ///<summary>配列をファイルに保存</summary>
            member this.save_text (f:double3,filename:string) = this.save_text(f,st filename)
            member this.save_text (f:double2,filename:string) = this.save_text(f,st filename)
            member this.save_text (f:double1,filename:string) = this.save_text(f,st filename)
            member this.save_text (f:double0,filename:string) = this.save_text(f,st filename)
            ///<summary>配列をファイルに保存</summary>
            member this.save_text (f:complex3,filename:string) = this.save_text(f,st filename)
            member this.save_text (f:complex2,filename:string) = this.save_text(f,st filename)
            member this.save_text (f:complex1,filename:string) = this.save_text(f,st filename)
            member this.save_text (f:complex0,filename:string) = this.save_text(f,st filename)
    
            ///<summary>数値をファイルに保存</summary>
            member private this.save (f:expr,filename:exprString) =
                this.binfileOutput filename <| fun w ->
                    //データフォーマット
                    w.b _1
                    //データ型
                    match f.etype with
                    |Etype.It 4  -> w.b 1004
                    |Etype.Dt    -> w.b 2000
                    |Etype.Zt    -> w.b 3000
                    |_           -> w.b 0
                    //データ次元
                    w.b _0
                    //データサイズ
                    w.b _1
                    //データ本体
                    match f.etype with
                    |Zt ->
                        w.b (complex0 f).re
                        w.b (complex0 f).im
                    |Dt ->
                        w.b (double0 f)
                    |It 4 ->
                        w.b (int0 f)
                    |_ -> ()
            member private this.save (f:int0,filename:exprString) = this.save (f.Expr,filename)
            member private this.save (f:double0,filename:exprString) = this.save (f.Expr,filename)
            member private this.save (f:complex0,filename:exprString) = this.save (f.Expr,filename)
    
            ///<summary>1次元データをファイルに保存</summary>
            member this.save (f:int1,filename:exprString) =
                    this.binfileOutput filename <| fun w ->
                        //データフォーマット
                        w.b _1
                        //データ型
                        match f.etype with
                        |Etype.It 4 -> w.b 1004
                        |Etype.Dt   -> w.b 2000
                        |Etype.Zt   -> w.b 3000
                        |_          -> w.b 0
                        //データ次元
                        w.b _1
                        //データサイズ
                        w.b f.size1
                        //データ本体
                        this.GenerationContext.iter.num f.size1 <| fun i ->
                            w.b f[i]
            ///<summary>1次元データをファイルに保存</summary>
            member this.save (f:double1,filename:exprString) =
                    this.binfileOutput filename <| fun w ->
                        //データフォーマット
                        w.b _1
                        //データ型
                        match f.etype with
                        |Etype.It 4 -> w.b 1004
                        |Etype.Dt   -> w.b 2000
                        |Etype.Zt   -> w.b 3000
                        |_          -> w.b 0
                        //データ次元
                        w.b _1
                        //データサイズ
                        w.b f.size1
                        //データ本体
                        this.GenerationContext.iter.num f.size1 <| fun i ->
                            w.b f[i]
            ///<summary>1次元データをファイルに保存</summary>
            member this.save (f:complex1,filename:exprString) =
                    this.binfileOutput filename <| fun w ->
                        //データフォーマット
                        w.b _1
                        //データ型
                        match f.etype with
                        |Etype.It 4  -> w.b 1004
                        |Etype.Dt    -> w.b 2000
                        |Etype.Zt    -> w.b 3000
                        |_           -> w.b 0
                        //データ次元
                        w.b _1
                        //データサイズ
                        w.b f.size1
                        //データ本体
                        this.GenerationContext.iter.num f.size1 <| fun i ->
                            w.b f[i].re
                            w.b f[i].im
    
            ///<summary>2次元データをファイルに保存</summary>
            member this.save (f:int2,filename:exprString) =
                this.binfileOutput filename <| fun w ->
                    //データフォーマット
                    w.b _1
                    //データ型
                    match f.etype with
                    |Etype.It 4 -> w.b 1004
                    |Etype.Dt   -> w.b 2000
                    |Etype.Zt   -> w.b 3000
                    |_          -> w.b 0
                    //データ次元
                    w.b _2
                    //データサイズ
                    w.b f.size1
                    w.b f.size2
                    //データ本体
                    this.GenerationContext.iter.num f.size2 <| fun j ->
                        this.GenerationContext.iter.num f.size1 <| fun i ->
                            w.b f[i,j]
    
    
            ///<summary>2次元データをファイルに保存</summary>
            member this.save (f:double2,filename:exprString) =
                this.binfileOutput filename <| fun w ->
                    //データフォーマット
                    w.b _1
                    //データ型
                    match f.etype with
                    |Etype.It 4 -> w.b 1004
                    |Etype.Dt   -> w.b 2000
                    |Etype.Zt   -> w.b 3000
                    |_          -> w.b 0
                    //データ次元
                    w.b _2
                    //データサイズ
                    w.b f.size1
                    w.b f.size2
                    //データ本体
                    this.GenerationContext.iter.num f.size2 <| fun j ->
                        this.GenerationContext.iter.num f.size1 <| fun i ->
                            w.b f[i,j]
    
    
            ///<summary>2次元データをファイルに保存</summary>
            member this.save (f:complex2,filename:exprString) =
                this.binfileOutput filename <| fun w ->
                    //データフォーマット
                    w.b _1
                    //データ型
                    match f.etype with
                    |Etype.It 4 -> w.b 1004
                    |Etype.Dt   -> w.b 2000
                    |Etype.Zt   -> w.b 3000
                    |_          -> w.b 0
                    //データ次元
                    w.b _2
                    //データサイズ
                    w.b f.size1
                    w.b f.size2
                    //データ本体
                    this.GenerationContext.iter.num f.size2 <| fun j ->
                        this.GenerationContext.iter.num f.size1 <| fun i ->
                            w.b f[i,j].re
                            w.b f[i,j].im
    
            ///<summary>3次元データをファイルに保存</summary>
            member this.save (f:int3,filename:exprString) =
                this.binfileOutput filename <| fun w ->
                    //データフォーマット
                    w.b _1
                    //データ型
                    match f.etype with
                    |Etype.It 4 -> w.b 1004
                    |Etype.Dt   -> w.b 2000
                    |Etype.Zt   -> w.b 3000
                    |_          -> w.b 0
                    //データ次元
                    w.b _3
                    //データサイズ
                    w.b f.size1
                    w.b f.size2
                    w.b f.size3
                    //データ本体
                    this.GenerationContext.iter.num f.size3 <| fun k ->
                        this.GenerationContext.iter.num f.size2 <| fun j ->
                            this.GenerationContext.iter.num f.size1 <| fun i ->
                                w.b f[i,j,k]
    
            ///<summary>3次元データをファイルに保存</summary>
            member this.save (f:double3,filename:exprString) =
                this.binfileOutput filename <| fun w ->
                    //データフォーマット
                    w.b _1
                    //データ型
                    match f.etype with
                    |Etype.It 4 -> w.b 1004
                    |Etype.Dt   -> w.b 2000
                    |Etype.Zt   -> w.b 3000
                    |_          -> w.b 0
                    //データ次元
                    w.b _3
                    //データサイズ
                    w.b f.size1
                    w.b f.size2
                    w.b f.size3
                    //データ本体
                    this.GenerationContext.iter.num f.size3 <| fun k ->
                        this.GenerationContext.iter.num f.size2 <| fun j ->
                            this.GenerationContext.iter.num f.size1 <| fun i ->
                                w.b f[i,j,k]
    
            ///<summary>3次元データをファイルに保存</summary>
            member this.save (f:complex3,filename:exprString) =
                this.binfileOutput filename <| fun w ->
                    //データフォーマット
                    w.b _1
                    //データ型
                    match f.etype with
                    |Etype.It 4 -> w.b 1004
                    |Etype.Dt   -> w.b 2000
                    |Etype.Zt   -> w.b 3000
                    |_          -> w.b 0
                    //データ次元
                    w.b _3
                    //データサイズ
                    w.b f.size1
                    w.b f.size2
                    w.b f.size3
                    //データ本体
                    this.GenerationContext.iter.num f.size3 <| fun k ->
                        this.GenerationContext.iter.num f.size2 <| fun j ->
                            this.GenerationContext.iter.num f.size1 <| fun i ->
                                w.b f[i,j,k].re
                                w.b f[i,j,k].im
            member this.save (f:int3,filename:string) = this.save(f,st filename)
            member this.save (f:int2,filename:string) = this.save(f,st filename)
            member this.save (f:int1,filename:string) = this.save(f,st filename)
            member this.save (f:int0,filename:string) = this.save(f,st filename)
            member this.save (f:double3,filename:string) = this.save(f,st filename)
            member this.save (f:double2,filename:string) = this.save(f,st filename)
            member this.save (f:double1,filename:string) = this.save(f,st filename)
            member this.save (f:double0,filename:string) = this.save(f,st filename)
            member this.save (f:complex3,filename:string) = this.save(f,st filename)
            member this.save (f:complex2,filename:string) = this.save(f,st filename)
            member this.save (f:complex1,filename:string) = this.save(f,st filename)
            member this.save (f:complex0,filename:string) = this.save(f,st filename)
