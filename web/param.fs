// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis

/// 水平方向アライメント
type BorderH = |L |C |R |J |Ll |Cl |Rl |Jl |Lr |Cr |Rr |Jr |Llr |Clr |Rlr |Jlr

/// 垂直方向アライメント
type BorderV = |TB |T |B |N

/// ファイル読み込みのオプション
type FileFlag =
    ///include_path のファイルを検索
    |FILE_USE_INCLUDE_PATH
    ///配列の各要素の最後の改行を省略
    |FILE_IGNORE_NEW_LINES
    ///空行を読み飛ばす
    |FILE_SKIP_EMPTY_LINES
    ///デフォルトのストリームコンテキストを使用しない
    |FILE_NO_DEFAULT_CONTEXT
    ///オプションのコード生成
    member this.str with get() =
        match this with
        |FILE_USE_INCLUDE_PATH -> "FILE_USE_INCLUDE_PATH"
        |FILE_IGNORE_NEW_LINES -> "FILE_IGNORE_NEW_LINES"
        |FILE_SKIP_EMPTY_LINES -> "FILE_SKIP_EMPTY_LINES"
        |FILE_NO_DEFAULT_CONTEXT -> "FILE_NO_DEFAULT_CONTEXT"
        
///ファイルの入出力モード
type FileOpenMode =
    ///書き込み専用、ファイルがない場合：エラー、ファイルポインタ：先頭
    |Wr
    ///読み・書き込み専用、ファイルがない場合：新規作成、ファイルポインタ：先頭
    |Wrp
    ///読み込み専用、ファイルがない場合：エラー、ファイルポインタ：先頭
    |Rd
    ///読み・書き込み専用、ファイルがない場合：エラー、ファイルポインタ：先頭
    |Rdp
    ///書き込み専用、ファイルがない場合：エラー、ファイルポインタ：先頭
    |Xs
    ///読み・書き込み専用、ファイルがない場合：エラー、ファイルポインタ：先頭
    |Xsp
    ///追記専用、ファイルがない場合：新規作成、ファイルポインタ：末尾
    |Ad
    ///読み・書き込み専用、ファイルがない場合：新規作成、ファイルポインタ：末尾
    |Adp
    ///書き込み専用、ファイルがない場合：新規作成、ファイルポインタ：先頭
    |Nw
    ///読み・書き込み専用、ファイルがない場合：新規作成、ファイルポインタ：先頭
    |Nwp
    member this.str with get() =
        match this with
        |Wr  -> "\"w\""
        |Wrp -> "\"w+\""
        |Rd  -> "\"r\""
        |Rdp -> "\"r+\""
        |Xs  -> "\"x\""
        |Xsp -> "\"x+\""
        |Ad  -> "\"a\""
        |Adp -> "\"a+\""
        |Nw  -> "\"c\""
        |Nwp -> "\"c+\""