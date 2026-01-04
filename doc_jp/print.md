[目次へ戻る](index.md)
## 画面出力

`print`を用いて変数の値を画面に出力できる。
```fsharp
print.c x
```
`print.`の後に変数の数を指定する(ドットを忘れないように注意)。指定できる変数の数は最大で**4個まで**。つまり、
`print.c`、`print.cc`、`print.ccc`、`print.cccc`の4パターンある。
```fsharp
print.cc x y
print.ccc x y z
print.cccc x y z w
```
5個以上の変数を出力する場合は以下のようにする。「`s`」と「`[`」の間には半角スペースが必要。変数はセミコロンで区切る。
```
print.n [a; b; c; d; e;]
```
文字と変数の両方を出力する場合は`++`演算子で結合して以下のように記述する。
```
print.w <| "aaa"++a++"bbb"++b
```
