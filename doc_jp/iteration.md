[目次へ戻る](index.md)
## 反復処理 

以下のコードで反復処理を指定する

`i`は整数型の変数で、`i`が1から10まで1ずつ増加しながら処理が繰り返し実行される。反復処理の対象はこのコードの次の行以降。反復処理の範囲はインデントで表わす。
```fsharp
iter.range 1 10 <| fun i ->
```

以下の例では`aaa`が画面に10回表示される。「`print.c !."bbb"`」の行はインデントが戻っているため10回の反復処理の後に1度だけ実行される
```fsharp
iter.range 1 10 <| fun i ->
    print.c !."aaa"
print.c !."bbb"
```
以下のコードでは配列`a`の第`n`要素に`n`を代入する
```fsharp
iter.range 1 a.size1 <| fun n ->
    a[n] <== n
```
ループ変数の開始値が1の時は以下のようにも記述できる
```fsharp
iter.num a.size1 <| fun n ->
    a[n] <== n
```
配列の全要素にアクセスする場合など、反復処理の範囲が1から`a.size1`のような場合は以下のように記述できる。
```fsharp
a.foreach <| fun n ->
    a[n] <== n
```
反復処理の中に反復処理を記述することも可能。2次元配列の処理などで利用される。
```fsharp
iter.num 5 <| fun i ->
    iter.num 10 <| fun j ->
        print.cc i j
```
2次元配列の全要素にアクセスする場合は以下のように記述できる。
```fsharp
a.foreach <| fun (i,j) ->
    a[i,j] <== i*j
```
`iter.loop`は無限ループ。`ex`はループを脱出する関数で、if式と組み合わせて使用される。`i`はループカウンタ
```fsharp
iter.loop <| fun (ex,i) ->
    print.t "aaa"
    x <== x - i*i
    br.if1 (i.>100) <| fun () ->
        ex() //ここでループ脱出
```
条件を満たす限り反復を繰り返す。
```fsharp
iter.whiledo (条件) <| fun ex ->
    (コード)
```

リストの各要素に対し処理を行う場合は`iter.list`を使用する。
```fsharp
iter.list [x;y;z] <| fun v ->
    print.c v
```
これは以下と同じ動作になる
```fsharp
print.c x
print.c y
print.c z
```