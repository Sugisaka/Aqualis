[目次へ戻る](index.md)
## 画面出力

`print`を用いて変数の値を画面に出力できる。
```fsharp
print.c x
```
`print.`の後に変数の数を指定する(ドットを忘れないように注意)。指定できる変数の数は最大で**4個まで**。つまり、
`print.c`、`print.cc`、`print.ccc`、`print.cccc`の4パターンある。
```fsharp
print.ccc x y z
```
変数は半角スペースで区切る。文字を出力するときは以下のようにする。
```fsharp
print.t "abc"
print.c !."abc"
print.cc x !."abc"
```
5個以上の変数を出力する場合は以下のようにする。
```
print.s [a; b; c; d; e;]
```
「`s`」と「`[`」の間には半角スペースが必要。変数はセミコロンで区切る。