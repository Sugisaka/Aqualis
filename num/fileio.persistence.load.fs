//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    [<AutoOpen>]
    module ContextIoLoadExtensions =
        let private failInvalidPersistenceData (context:Aqualis) message =
            match context.language with
            | C99 ->
                context.codewritein("fprintf(stderr, " + OutputTextLiteral.c ("Aqualis: " + message + "\n") + "); exit(EXIT_FAILURE);\n")
            | Fortran ->
                context.codewritein("error stop " + OutputTextLiteral.fortran ("Aqualis: " + message) + "\n")
            | Python ->
                context.codewritein("raise ValueError(" + OutputTextLiteral.python ("Aqualis: " + message) + ")\n")
            | _ -> context.print.s message

        let private requireFormatVersion (context:Aqualis) (version:int0) =
            context.br.if1 (version .=/ 1) (fun () -> failInvalidPersistenceData context "invalid data format")

        let private requireScalarSize (context:Aqualis) (size:int0) =
            context.br.if1 (size .=/ 1) (fun () -> failInvalidPersistenceData context "invalid scalar data size")

        let private requireArrayPayload (reader:BinReader) (typeCode:int) (dimensions:int0 list) =
            let bytesPerElement =
                match typeCode with
                | 1004 -> 4
                | 2000 -> 8
                | 3000 -> 16
                | _ -> invalidArg (nameof typeCode) "Unsupported persistence element type."
            reader.RequireArrayPayload(dimensions,bytesPerElement)

        let private requireArrayAllocation (context:Aqualis) (arrayCode:string) (dimensions:int0 list) =
            if context.language = C99 then
                let nonEmpty = dimensions |> List.map (fun size -> "(" + size.Expr.eval context + " > 0)") |> String.concat " && "
                context.codewritein("if (" + nonEmpty + " && " + arrayCode + " == NULL) { fprintf(stderr, \"Aqualis: failed to allocate array data.\\n\"); exit(EXIT_FAILURE); }\n")

        type ContextIo with
            ///<summary>数値をファイルから読み込み</summary>
            member this.load (f:int0,filename:exprString) =
                let reader (r:BinReader) (nt:int,t:Etype) =
                    this.GenerationContext.ch.i <| fun n ->
                        //データ型
                        r.b n
                        this.GenerationContext.br.if2 (n.=nt)
                            <| fun () ->
                                //データ次元
                                r.b n
                                this.GenerationContext.br.if2 (n.=0)
                                    <| fun () ->
                                        this.GenerationContext.ch.i <| fun n1 ->
                                            //データサイズ
                                            r.b n1
                                            requireScalarSize this.GenerationContext n1
                                            //データ本体
                                            r.b f
                                    <| fun () ->
                                        failInvalidPersistenceData this.GenerationContext "invalid data dimension"
                            <| fun () ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
                this.binfileInput filename <| fun r ->
                this.GenerationContext.ch.i <| fun n ->
                    //データフォーマット
                    r.b n
                    requireFormatVersion this.GenerationContext n
                    this.GenerationContext.br.branch <| fun b ->
                        b.IF (n.=1) <| fun () ->
                            match f.etype with
                            |It 4 ->
                                reader r (1004,f.etype)
                            |Dt    ->
                                reader r (2000,f.etype)
                            |Zt    ->
                                reader r (3000,f.etype)
                            |_ ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
    
            ///<summary>数値をファイルから読み込み</summary>
            member this.load (f:double0,filename:exprString) =
                let reader (r:BinReader) (nt:int,t:Etype) =
                    this.GenerationContext.ch.i <| fun n ->
                        //データ型
                        r.b n
                        this.GenerationContext.br.if2 (n.=nt)
                            <| fun () ->
                                //データ次元
                                r.b n
                                this.GenerationContext.br.if2 (n.=0)
                                    <| fun () ->
                                        this.GenerationContext.ch.i <| fun n1 ->
                                            //データサイズ
                                            r.b n1
                                            requireScalarSize this.GenerationContext n1
                                            //データ本体
                                            r.b f
                                    <| fun () ->
                                        failInvalidPersistenceData this.GenerationContext "invalid data dimension"
                            <| fun () ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
                this.binfileInput filename <| fun r ->
                this.GenerationContext.ch.i <| fun n ->
                    //データフォーマット
                    r.b n
                    requireFormatVersion this.GenerationContext n
                    this.GenerationContext.br.branch <| fun b ->
                        b.IF (n.=1) <| fun () ->
                            match f.etype with
                            |It 4 ->
                                reader r (1004,f.etype)
                            |Dt    ->
                                reader r (2000,f.etype)
                            |Zt    ->
                                reader r (3000,f.etype)
                            |_ ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
    
            ///<summary>数値をファイルから読み込み</summary>
            member this.load (f:complex0,filename:exprString) =
                let reader (r:BinReader) (nt:int,t:Etype) =
                    this.GenerationContext.ch.i <| fun n ->
                        //データ型
                        r.b n
                        this.GenerationContext.br.if2 (n.=nt)
                            <| fun () ->
                                //データ次元
                                r.b n
                                this.GenerationContext.br.if2 (n.=0)
                                    <| fun () ->
                                        this.GenerationContext.ch.i <| fun n1 ->
                                            //データサイズ
                                            r.b n1
                                            requireScalarSize this.GenerationContext n1
                                            //データ本体
                                            this.GenerationContext.ch.dd <| fun (re,im) ->
                                                r.b re
                                                r.b im
                                                f <== re + asm.uj*im
                                    <| fun () ->
                                        failInvalidPersistenceData this.GenerationContext "invalid data dimension"
                            <| fun () ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
                this.binfileInput filename <| fun r ->
                this.GenerationContext.ch.i <| fun n ->
                    //データフォーマット
                    r.b n
                    requireFormatVersion this.GenerationContext n
                    this.GenerationContext.br.branch <| fun b ->
                        b.IF (n.=1) <| fun () ->
                            match f.etype with
                            |It 4 ->
                                reader r (1004,f.etype)
                            |Dt    ->
                                reader r (2000,f.etype)
                            |Zt    ->
                                reader r (3000,f.etype)
                            |_ ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
    
            ///<summary>1次元データをファイルから読み込み</summary>
            member this.load (f:int1,filename:exprString) =
                let reader (r:BinReader) (nt:int,t:Etype) =
                    this.GenerationContext.ch.i <| fun n ->
                        //データ型
                        r.b n
                        this.GenerationContext.br.if2 (n.=nt)
                            <| fun () ->
                                //データ次元
                                r.b n
                                this.GenerationContext.br.if2 (n.=1)
                                    <| fun () ->
                                        this.GenerationContext.ch.i <| fun n1 ->
                                            //データサイズ
                                            r.b n1
                                            requireArrayPayload r nt [n1]
                                            f.allocate n1
                                            requireArrayAllocation this.GenerationContext f.code [n1]
                                            //データ本体
                                            this.GenerationContext.iter.num f.size1 <| fun i ->
                                                this.GenerationContext.ch.i <| fun u ->
                                                    r.b u
                                                    f[i] <== u
                                    <| fun () ->
                                        failInvalidPersistenceData this.GenerationContext "invalid data dimension"
                            <| fun () ->
                                failInvalidPersistenceData this.GenerationContext ": invalid data type"
                this.binfileInput filename <| fun r ->
                this.GenerationContext.ch.i <| fun n ->
                    //データフォーマット
                    r.b n
                    requireFormatVersion this.GenerationContext n
                    this.GenerationContext.br.branch <| fun b ->
                        b.IF (n.=1) <| fun () ->
                            match f[0].etype with
                            |Etype.It 4  ->
                                reader r (1004,f[0].etype)
                            |Etype.Dt    ->
                                reader r (2000,f[0].etype)
                            |Etype.Zt    ->
                                reader r (3000,f[0].etype)
                            |_ ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
    
            ///<summary>1次元データをファイルから読み込み</summary>
            member this.load (f:double1,filename:exprString) =
                let reader (r:BinReader) (nt:int,t:Etype) =
                    this.GenerationContext.ch.i <| fun n ->
                        //データ型
                        r.b n
                        this.GenerationContext.br.if2 (n.=nt)
                            <| fun () ->
                                //データ次元
                                r.b n
                                this.GenerationContext.br.if2 (n.=1)
                                    <| fun () ->
                                        this.GenerationContext.ch.i <| fun n1 ->
                                            //データサイズ
                                            r.b n1
                                            requireArrayPayload r nt [n1]
                                            f.allocate n1
                                            requireArrayAllocation this.GenerationContext f.code [n1]
                                            //データ本体
                                            this.GenerationContext.iter.num f.size1 <| fun i ->
                                                this.GenerationContext.ch.d <| fun u ->
                                                    r.b u
                                                    f[i] <== u
                                    <| fun () ->
                                        failInvalidPersistenceData this.GenerationContext "invalid data dimension"
                            <| fun () ->
                                failInvalidPersistenceData this.GenerationContext ": invalid data type"
                this.binfileInput filename <| fun r ->
                this.GenerationContext.ch.i <| fun n ->
                    //データフォーマット
                    r.b n
                    requireFormatVersion this.GenerationContext n
                    this.GenerationContext.br.branch <| fun b ->
                        b.IF (n.=1) <| fun () ->
                            match f[0].etype with
                            |Etype.It 4  ->
                                reader r (1004,f[0].etype)
                            |Etype.Dt    ->
                                reader r (2000,f[0].etype)
                            |Etype.Zt    ->
                                reader r (3000,f[0].etype)
                            |_ ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
    
            ///<summary>1次元データをファイルから読み込み</summary>
            member this.load (f:complex1,filename:exprString) =
                let reader (r:BinReader) (nt:int,t:Etype) =
                    this.GenerationContext.ch.i <| fun n ->
                        //データ型
                        r.b n
                        this.GenerationContext.br.if2 (n.=nt)
                            <| fun () ->
                                //データ次元
                                r.b n
                                this.GenerationContext.br.if2 (n.=1)
                                    <| fun () ->
                                        this.GenerationContext.ch.i <| fun n1 ->
                                            //データサイズ
                                            r.b n1
                                            requireArrayPayload r nt [n1]
                                            f.allocate n1
                                            requireArrayAllocation this.GenerationContext f.code [n1]
                                            //データ本体
                                            match t with
                                            |It _ ->
                                                this.GenerationContext.iter.num f.size1 <| fun i ->
                                                    this.GenerationContext.ch.i <| fun u ->
                                                        r.b u
                                                        f[i] <== u
                                            |Dt ->
                                                this.GenerationContext.iter.num f.size1 <| fun i ->
                                                    this.GenerationContext.ch.d <| fun u ->
                                                        r.b u
                                                        f[i] <== u
                                            |Zt ->
                                                this.GenerationContext.iter.num f.size1 <| fun i ->
                                                    this.GenerationContext.ch.dd <| fun (re,im) ->
                                                        r.b re
                                                        r.b im
                                                        f[i] <== re + asm.uj*im
                                            |_ ->
                                                ()
                                    <| fun () ->
                                        failInvalidPersistenceData this.GenerationContext "invalid data dimension"
                            <| fun () ->
                                failInvalidPersistenceData this.GenerationContext ": invalid data type"
                this.binfileInput filename <| fun r ->
                this.GenerationContext.ch.i <| fun n ->
                    //データフォーマット
                    r.b n
                    requireFormatVersion this.GenerationContext n
                    this.GenerationContext.br.branch <| fun b ->
                        b.IF (n.=1) <| fun () ->
                            match f[0].etype with
                            |Etype.It 4  ->
                                reader r (1004,f[0].etype)
                            |Etype.Dt    ->
                                reader r (2000,f[0].etype)
                            |Etype.Zt    ->
                                reader r (3000,f[0].etype)
                            |_ ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
    
            ///<summary>2次元データをファイルから読み込み</summary>
            member this.load (f:int2,filename:exprString) =
                let reader (r:BinReader) (nt:int,t:Etype) =
                    this.GenerationContext.ch.i <| fun n ->
                        //データ型
                        r.b n
                        this.GenerationContext.br.if2 (n.=nt)
                            <| fun () ->
                                //データ次元
                                r.b n
                                this.GenerationContext.br.if2 (n.=2)
                                    <| fun () ->
                                        this.GenerationContext.ch.ii <| fun (n1,n2) ->
                                            //データサイズ
                                            r.b n1
                                            r.b n2
                                            requireArrayPayload r nt [n1;n2]
                                            f.allocate(n1,n2)
                                            requireArrayAllocation this.GenerationContext f.code [n1;n2]
                                            //データ本体
                                            this.GenerationContext.iter.num f.size2 <| fun j ->
                                                this.GenerationContext.iter.num f.size1 <| fun i ->
                                                    this.GenerationContext.ch.i <| fun u ->
                                                        r.b u
                                                        f[i,j] <== u
                                    <| fun () ->
                                        failInvalidPersistenceData this.GenerationContext "invalid data dimension"
                            <| fun () ->
                                failInvalidPersistenceData this.GenerationContext ": invalid data type"
                                this.GenerationContext.print.tt <| n++(I nt)
                this.binfileInput filename <| fun r ->
                this.GenerationContext.ch.i <| fun n ->
                    //データフォーマット
                    r.b n
                    requireFormatVersion this.GenerationContext n
                    this.GenerationContext.br.branch <| fun b ->
                        b.IF (n.=1) <| fun () ->
                            match f[0,0].etype with
                            |It 4 ->
                                reader r (1004,f[0,0].etype)
                            |Dt   ->
                                reader r (2000,f[0,0].etype)
                            |Zt   ->
                                reader r (3000,f[0,0].etype)
                            |_ ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
    
            ///<summary>2次元データをファイルから読み込み</summary>
            member this.load (f:double2,filename:exprString) =
                let reader (r:BinReader) (nt:int,t:Etype) =
                    this.GenerationContext.ch.i <| fun n ->
                        //データ型
                        r.b n
                        this.GenerationContext.br.if2 (n.=nt)
                            <| fun () ->
                                //データ次元
                                r.b n
                                this.GenerationContext.br.if2 (n.=2)
                                    <| fun () ->
                                        this.GenerationContext.ch.ii <| fun (n1,n2) ->
                                            //データサイズ
                                            r.b n1
                                            r.b n2
                                            requireArrayPayload r nt [n1;n2]
                                            f.allocate(n1,n2)
                                            requireArrayAllocation this.GenerationContext f.code [n1;n2]
                                            //データ本体
                                            this.GenerationContext.iter.num f.size2 <| fun j ->
                                                this.GenerationContext.iter.num f.size1 <| fun i ->
                                                    this.GenerationContext.ch.d <| fun u ->
                                                        r.b u
                                                        f[i,j] <== u
                                    <| fun () ->
                                        failInvalidPersistenceData this.GenerationContext "invalid data dimension"
                            <| fun () ->
                                failInvalidPersistenceData this.GenerationContext ": invalid data type"
                                this.GenerationContext.print.tt <| n++(I nt)
                this.binfileInput filename <| fun r ->
                this.GenerationContext.ch.i <| fun n ->
                    //データフォーマット
                    r.b n
                    requireFormatVersion this.GenerationContext n
                    this.GenerationContext.br.branch <| fun b ->
                        b.IF (n.=1) <| fun () ->
                            match f[0,0].etype with
                            |It 4 ->
                                reader r (1004,f[0,0].etype)
                            |Dt   ->
                                reader r (2000,f[0,0].etype)
                            |Zt   ->
                                reader r (3000,f[0,0].etype)
                            |_ ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
    
            ///<summary>2次元データをファイルから読み込み</summary>
            member this.load (f:complex2,filename:exprString) =
                let reader (r:BinReader) (nt:int,t:Etype) =
                    this.GenerationContext.ch.i <| fun n ->
                        //データ型
                        r.b n
                        this.GenerationContext.br.if2 (n.=nt)
                            <| fun () ->
                                //データ次元
                                r.b n
                                this.GenerationContext.br.if2 (n.=2)
                                    <| fun () ->
                                        this.GenerationContext.ch.ii <| fun (n1,n2) ->
                                            //データサイズ
                                            r.b n1
                                            r.b n2
                                            requireArrayPayload r nt [n1;n2]
                                            f.allocate(n1,n2)
                                            requireArrayAllocation this.GenerationContext f.code [n1;n2]
                                            //データ本体
                                            match t with
                                            |It _ ->
                                                this.GenerationContext.iter.num f.size2 <| fun j ->
                                                    this.GenerationContext.iter.num f.size1 <| fun i ->
                                                        this.GenerationContext.ch.i <| fun u ->
                                                            r.b u
                                                            f[i,j] <== u
                                            |Dt ->
                                                this.GenerationContext.iter.num f.size2 <| fun j ->
                                                    this.GenerationContext.iter.num f.size1 <| fun i ->
                                                        this.GenerationContext.ch.d <| fun u ->
                                                            r.b u
                                                            f[i,j] <== u
                                            |Zt ->
                                                this.GenerationContext.iter.num f.size2 <| fun j ->
                                                    this.GenerationContext.iter.num f.size1 <| fun i ->
                                                        this.GenerationContext.ch.dd <| fun (re,im) ->
                                                            r.b re
                                                            r.b im
                                                            f[i,j] <== re + asm.uj*im
                                            |_ ->
                                                ()
                                    <| fun () ->
                                        failInvalidPersistenceData this.GenerationContext "invalid data dimension"
                            <| fun () ->
                                failInvalidPersistenceData this.GenerationContext ": invalid data type"
                                this.GenerationContext.print.tt <| n++(I nt)
                this.binfileInput filename <| fun r ->
                this.GenerationContext.ch.i <| fun n ->
                    //データフォーマット
                    r.b n
                    requireFormatVersion this.GenerationContext n
                    this.GenerationContext.br.branch <| fun b ->
                        b.IF (n.=1) <| fun () ->
                            match f[0,0].etype with
                            |It 4 ->
                                reader r (1004,f[0,0].etype)
                            |Dt   ->
                                reader r (2000,f[0,0].etype)
                            |Zt   ->
                                reader r (3000,f[0,0].etype)
                            |_ ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
    
            ///<summary>3次元データをファイルから読み込み</summary>
            member this.load (f:int3,filename:exprString) =
                let reader (r:BinReader) (nt:int,t:Etype) =
                    this.GenerationContext.ch.i <| fun n ->
                        //データ型
                        r.b n
                        this.GenerationContext.br.if2 (n.=nt)
                            <| fun () ->
                                //データ次元
                                r.b n
                                this.GenerationContext.br.if2 (n.=3)
                                    <| fun () ->
                                        this.GenerationContext.ch.iii <| fun (n1,n2,n3) ->
                                            //データサイズ
                                            r.b n1
                                            r.b n2
                                            r.b n3
                                            requireArrayPayload r nt [n1;n2;n3]
                                            f.allocate(n1,n2,n3)
                                            requireArrayAllocation this.GenerationContext f.code [n1;n2;n3]
                                            //データ本体
                                            this.GenerationContext.iter.num f.size3 <| fun k ->
                                                this.GenerationContext.iter.num f.size2 <| fun j ->
                                                    this.GenerationContext.iter.num f.size1 <| fun i ->
                                                        this.GenerationContext.ch.i <| fun u ->
                                                            r.b u
                                                            f[i,j,k] <== u
                                    <| fun () ->
                                        failInvalidPersistenceData this.GenerationContext "invalid data dimension"
                            <| fun () ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
                this.binfileInput filename <| fun r ->
                this.GenerationContext.ch.i <| fun n ->
                    //データフォーマット
                    r.b n
                    requireFormatVersion this.GenerationContext n
                    this.GenerationContext.br.branch <| fun b ->
                        b.IF (n.=1) <| fun () ->
                            match f[_0,_0,_0].etype with
                            |Etype.It 4  ->
                                reader r (1004,f[_0,_0,_0].etype)
                            |Etype.Dt    ->
                                reader r (2000,f[_0,_0,_0].etype)
                            |Etype.Zt    ->
                                reader r (3000,f[_0,_0,_0].etype)
                            |_ ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
    
            member this.load (f:int3,filename:string) = this.load(f,st filename)
            member this.load (f:int2,filename:string) = this.load(f,st filename)
            member this.load (f:int1,filename:string) = this.load(f,st filename)
            member this.load (f:int0,filename:string) = this.load(f,st filename)
    
            ///<summary>3次元データをファイルから読み込み</summary>
            member this.load (f:double3,filename:exprString) =
                let reader (r:BinReader) (nt:int,t:Etype) =
                    this.GenerationContext.ch.i <| fun n ->
                        //データ型
                        r.b n
                        this.GenerationContext.br.if2 (n.=nt)
                            <| fun () ->
                                //データ次元
                                r.b n
                                this.GenerationContext.br.if2 (n.=3)
                                    <| fun () ->
                                        this.GenerationContext.ch.iii <| fun (n1,n2,n3) ->
                                            //データサイズ
                                            r.b n1
                                            r.b n2
                                            r.b n3
                                            requireArrayPayload r nt [n1;n2;n3]
                                            f.allocate(n1,n2,n3)
                                            requireArrayAllocation this.GenerationContext f.code [n1;n2;n3]
                                            //データ本体
                                            this.GenerationContext.iter.num f.size3 <| fun k ->
                                                this.GenerationContext.iter.num f.size2 <| fun j ->
                                                    this.GenerationContext.iter.num f.size1 <| fun i ->
                                                        this.GenerationContext.ch.d <| fun u ->
                                                            r.b u
                                                            f[i,j,k] <== u
                                    <| fun () ->
                                        failInvalidPersistenceData this.GenerationContext "invalid data dimension"
                            <| fun () ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
                this.binfileInput filename <| fun r ->
                this.GenerationContext.ch.i <| fun n ->
                    //データフォーマット
                    r.b n
                    requireFormatVersion this.GenerationContext n
                    this.GenerationContext.br.branch <| fun b ->
                        b.IF (n.=1) <| fun () ->
                            match f[_0,_0,_0].etype with
                            |Etype.It 4  ->
                                reader r (1004,f[_0,_0,_0].etype)
                            |Etype.Dt    ->
                                reader r (2000,f[_0,_0,_0].etype)
                            |Etype.Zt    ->
                                reader r (3000,f[_0,_0,_0].etype)
                            |_ ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
    
            member this.load (f:double3,filename:string) = this.load(f,st filename)
            member this.load (f:double2,filename:string) = this.load(f,st filename)
            member this.load (f:double1,filename:string) = this.load(f,st filename)
            member this.load (f:double0,filename:string) = this.load(f,st filename)
    
            ///<summary>3次元データをファイルから読み込み</summary>
            member this.load (f:complex3,filename:exprString) =
                let reader (r:BinReader) (nt:int,t:Etype) =
                    this.GenerationContext.ch.i <| fun n ->
                        //データ型
                        r.b n
                        this.GenerationContext.br.if2 (n.=nt)
                            <| fun () ->
                                //データ次元
                                r.b n
                                this.GenerationContext.br.if2 (n.=3)
                                    <| fun () ->
                                        this.GenerationContext.ch.iii <| fun (n1,n2,n3) ->
                                            //データサイズ
                                            r.b n1
                                            r.b n2
                                            r.b n3
                                            requireArrayPayload r nt [n1;n2;n3]
                                            f.allocate(n1,n2,n3)
                                            requireArrayAllocation this.GenerationContext f.code [n1;n2;n3]
                                            //データ本体
                                            this.GenerationContext.iter.num f.size3 <| fun k ->
                                                this.GenerationContext.iter.num f.size2 <| fun j ->
                                                    this.GenerationContext.iter.num f.size1 <| fun i ->
                                                        this.GenerationContext.ch.dd <| fun (re,im) ->
                                                            r.b re
                                                            r.b im
                                                            f[i,j,k] <== re + asm.uj*im
                                    <| fun () ->
                                        failInvalidPersistenceData this.GenerationContext "invalid data dimension"
                            <| fun () ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
                this.binfileInput filename <| fun r ->
                this.GenerationContext.ch.i <| fun n ->
                    //データフォーマット
                    r.b n
                    requireFormatVersion this.GenerationContext n
                    this.GenerationContext.br.branch <| fun b ->
                        b.IF (n.=1) <| fun () ->
                            match f[_0,_0,_0].etype with
                            |Etype.It 4  ->
                                reader r (1004,f[_0,_0,_0].etype)
                            |Etype.Dt    ->
                                reader r (2000,f[_0,_0,_0].etype)
                            |Etype.Zt    ->
                                reader r (3000,f[_0,_0,_0].etype)
                            |_ ->
                                failInvalidPersistenceData this.GenerationContext "invalid data type"
    
            member this.load (f:complex3,filename:string) = this.load(f,st filename)
            member this.load (f:complex2,filename:string) = this.load(f,st filename)
            member this.load (f:complex1,filename:string) = this.load(f,st filename)
            member this.load (f:complex0,filename:string) = this.load(f,st filename)
            
