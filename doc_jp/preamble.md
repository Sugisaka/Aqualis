[目次へ戻る](index.md)
## プリアンブル部 

fsxファイルの冒頭部分は毎回以下のように書く
```fsharp
//#############################################################################
// project title
let projectname = "template"
let version = "1.0.0"
//#############################################################################
 
let outputdir = @"C:\home\work"

#I "C:\\Aqualis\\lib\\184_0_1_0"
#r "Aqualis.dll"
#load "version.fsx"

let fullversion = preprocess.backup outputdir __SOURCE_DIRECTORY__ __SOURCE_FILE__ projectname version
 
open Aqualis
 
Compile [Fortran] outputdir projectname fullversion <| fun () ->
    (コード本体)
```

- 2行目：このプログラムの説明を書く。複数行になっても良いが、各行の先頭に半角「`//`」を記入
- 3行目：プロジェクト名を「`""`」の間に書く。半角のアルファベットと数字、アンダースコアが使用可能
- 4行目：バージョン番号。左からメジャー、マイナー、リビルド。リビジョン番号はこのソースファイルの実行時に自動的にナンバリングされる
- 7行目：ソースファイルの出力先フォルダ。
- 9行目：Aqualis.dllがあるディレクトリを指定する。
- 10行目：Aqualis.dllを読み込む
- 11行目：fsxファイルのバックアップを行う処理が書かれたプログラムを読み込む
- 13行目：このソースファイルのバックアップ処理を行い、リビジョン番号を決定したバージョン番号(`fullversion`)を決定する
- 15行目：Aqualisを使用可能にする
- 17行目：出力ソースファイルの言語を`[]`の中に指定。セミコロン`;`で区切って複数指定することもできる。以下の言語を指定可能
  - Fortran
  - C99
  - Python
  - LaTeX
  - HTML

以下のコードでは、「`print.c !."aaa"`」と「`print.c !."bbb"`」がFortranのコードに変換される。「`print.c !."ccc"`」はインデントが戻っているので出力の対象外となる。
```fsharp
Compile [Fortran] outputdir projectname fullversion <| fun () ->
    print.c !."aaa"
    print.c !."bbb"
print.c !."ccc"
```
