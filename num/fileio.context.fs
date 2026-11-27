//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System
    open System.Text

    /// File input and output operations for an Aqualis context.
    type ContextIo internal (ctx:Aqualis) =
        let writein text = ctx.codewritein(text + "\n")

        /// Gets the owning generation context.
        member internal _.GenerationContext = ctx

        ///<summary>ファイル出力（タブ区切りデータ）</summary>
        member this.fileOutput (filename:exprString) = fun code ->
            FileIoAccess.withFile ctx (filename,None) false false <| fun fp _ ->
                let writer = TextWriter(ctx,fp)
                code writer
        ///<summary>ファイル出力（タブ区切りデータ）</summary>
        member this.fileOutput (filename:exprString,intDigit:int) = fun code ->
            FileIoAccess.withFile ctx (filename,Some intDigit) false false <| fun fp _ ->
                let writer = TextWriter(ctx,fp)
                code writer

        ///<summary>ファイル出力（タブ区切りデータ）</summary>
        member this.fileOutput (filename:string) = fun code -> this.fileOutput (st filename) code

        ///<summary>バイナリファイル出力</summary>
        member this.binfileOutput (filename:exprString) = fun code ->
            FileIoAccess.withFile ctx (filename,None) false true <| fun fp _ ->
                let writer = BinWriter(ctx,fp)
                code writer
        ///<summary>バイナリファイル出力</summary>
        member this.binfileOutput (filename:exprString,intDigit:int) = fun code ->
            FileIoAccess.withFile ctx (filename,Some intDigit) false true <| fun fp _ ->
                let writer = BinWriter(ctx,fp)
                code writer

        ///<summary>バイナリファイル出力</summary>
        member this.binfileOutput (filename:string) = fun code -> this.binfileOutput (st filename) code

        ///<summary>ファイル読み込み</summary>
        member this.fileInput (filename:exprString) = fun code ->
            ctx.ch.i <| fun iostat ->
                if ctx.language = PHP then
                    code (TextReader(ctx,"",iostat,None))
                else
                    FileIoAccess.withFile ctx (filename,None) true false <| fun fp path ->
                        ctx.ch.f <| fun byteFp ->
                            let byteFile = if ctx.language = Fortran then Some(path,byteFp) else None
                            let reader = TextReader(ctx,fp,iostat,byteFile)
                            code reader
                            reader.CloseByteStream()

        ///<summary>ファイル読み込み</summary>
        member this.fileInput (filename:exprString,intDigit:int) = fun code ->
            ctx.ch.i <| fun iostat ->
                if ctx.language = PHP then
                    code (TextReader(ctx,"",iostat,None))
                else
                    FileIoAccess.withFile ctx (filename,Some intDigit) true false <| fun fp path ->
                        ctx.ch.f <| fun byteFp ->
                            let byteFile = if ctx.language = Fortran then Some(path,byteFp) else None
                            let reader = TextReader(ctx,fp,iostat,byteFile)
                            code reader
                            reader.CloseByteStream()

        ///<summary>ファイル読み込み</summary>
        member this.fileInput (filename:string) = fun code ->
            this.fileInput (st filename) code

        ///<summary>バイナリファイルの読み込み</summary>
        member this.binfileInput (filename:exprString) = fun code ->
            ctx.ch.i <| fun iostat ->
                FileIoAccess.withFile ctx (filename,None) true true <| fun fp _ ->
                    let reader = BinReader(ctx,fp,iostat)
                    code reader

        ///<summary>バイナリファイルの読み込み</summary>
        member this.binfileInput (filename:exprString,intDigit:int) = fun code ->
            ctx.ch.i <| fun iostat ->
                FileIoAccess.withFile ctx (filename,Some intDigit) true true <| fun fp _ ->
                    let reader = BinReader(ctx,fp,iostat)
                    code reader

        ///<summary>バイナリファイルの読み込み</summary>
        member this.binfileInput (filename:string) = fun code -> this.binfileInput (st filename) code

        ///<summary>ファイルの読み込み</summary>
        member this.file_Read (filename:exprString) = fun varlist code ->
            ctx.ch.i <| fun iostat ->
                FileIoAccess.withFile ctx (filename,None) true false <| fun fp _ ->
                    ctx.iter.loop <| fun (ext,i) ->
                        FileIoRecordReader.read ctx fp iostat varlist
                        if ctx.language = Fortran then
                            writein("if (" + iostat.code + " > 0) error stop 'Aqualis: invalid text input record.'\n")
                        ctx.br.branch <| fun b ->
                            b.IF (iostat .< 0) <| fun () ->
                                ext()
                            b.EL <| fun () ->
                                code(i)

        ///<summary>ファイルの読み込み</summary>
        member this.file_Read (filename:exprString,intDigit:int) = fun varlist code ->
            ctx.ch.i <| fun iostat ->
                FileIoAccess.withFile ctx (filename,Some intDigit) true false <| fun fp _ ->
                    ctx.iter.loop <| fun (ext,i) ->
                        FileIoRecordReader.read ctx fp iostat varlist
                        if ctx.language = Fortran then
                            writein("if (" + iostat.code + " > 0) error stop 'Aqualis: invalid text input record.'\n")
                        ctx.br.branch <| fun b ->
                            b.IF (iostat .< 0) <| fun () ->
                                ext()
                            b.EL <| fun () ->
                                code(i)
