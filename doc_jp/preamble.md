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

#I "C:\\Aqualis\\lib\\188_0_0_0"
#r "Aqualis.dll"
 
open Aqualis
 
Compile [Fortran] outputdir projectname version <| fun ctx ->
    (コード本体)
```

- 2行目：このプログラムの説明を書く。複数行になっても良いが、各行の先頭に半角「`//`」を記入
- 3行目：プロジェクト名を「`""`」の間に書く。半角のアルファベットと数字、アンダースコアが使用可能
- 4行目：バージョン番号。任意の文字列を指定可能
- 7行目：ソースファイルの出力先フォルダ。
- 9行目：Aqualis.dllがあるディレクトリを指定する。
- 10行目：Aqualis.dllを読み込む
- 12行目：Aqualisを使用可能にする
- 14行目：出力ソースファイルの言語を`[]`の中に指定。セミコロン`;`で区切って複数指定することもできる。以下の言語を指定可能
  - Fortran
  - C99
  - Python
  - LaTeX
  - HTML
- 14行目：`ctx`はコード生成に使用するコンテキスト。他の名称に変更してもよい。

以下のコードでは、「`print.t "aaa"`」と「`print.t "bbb"`」がFortranのコードに変換される。「`print.s "ccc"`」はインデントが戻っているので出力の対象外となる。
```fsharp
Compile [Fortran] outputdir projectname version <| fun ctx ->
    print.s "aaa"
    print.s "bbb"
print.s "ccc"
```
