[目次へ戻る](index.md)
## ファイルの読み書き 

コード上で出力した値を別のファイルに保存したり、また逆にファイルに保存されたデータを使いたい場合がある。以下にその方法を記す。

### ファイルへの書き込み 

変数`x`、`y`、`z`の値をファイル「`test.dat`」に書き込む
```fsharp
ch.id <| fun (x,y) ->
    x <== 1
    y <== 2.0
    io.fileOutput [!."test.dat"] <| fun wr ->
        wr [x; y] //セミコロンで区切っていくつでも指定可能
```
以下はエラーになる
```fsharp
let x = 1
let y = 2.0
io.fileOutput [!."test.dat"] <| fun wr ->
    wr [x; y]
```
括弧の中は`num0`型である必要があるが、`x`はint型、`y`はdouble型なのでエラーになる。以下のように書くとintやdoubleを強制的に`num0`に変換できる。
```fsharp
let x = 1
let y = 2.0
io.fileOutput [!."test.dat"] <| fun wr ->
    wr [I x; D y]
```
複数のファイルを同時に開くことも可能。その際は書き込み指定子「`wr`」の名前を変える
```fsharp
ch.id <| fun (x,y) ->
    x <== 1
    y <== 2.0
    io.fileOutput [!."test1.dat"] <| fun wr1 ->
        io.fileOutput [!."test2.dat"] <| fun wr2 ->
            wr1 [x] //test1.datに書き込み
            wr2 [x] //test2.datに書き込み
```
ファイル名は整数の変数を指定することも可能
```fsharp
ch.i <| fun n ->
    n <== 4
    io.fileOutput [!."test"; n; ".dat"] <| fun wr -> //ファイル名は「test00004.dat」
```

### ファイルからの読み込み 

以下のような内容のテキストファイル"test.dat"があるとき、
```
       3    -1.230000000000000E+001
```
ファイルの読み込みは以下のようにする。
```fsharp
ch.id <| fun (x,y) ->
    io.fileInput [!."test.dat"] <| fun rd ->
        rd [x; y]
```