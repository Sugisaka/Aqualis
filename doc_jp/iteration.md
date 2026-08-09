[目次へ戻る](index.md)
## 反復処理 

以下のコードで反復処理を指定する

`i`は整数型の変数で、`i`が1から10まで1ずつ増加しながら処理が繰り返し実行される。反復処理の対象はこのコードの次の行以降。反復処理の範囲はインデントで表わす。
```fsharp
ctx.iter.range (1, 10) <| fun i ->
```

以下の例では`aaa`が画面に10回表示される。「`print.t "bbb"`」の行はインデントが戻っているため10回の反復処理の後に1度だけ実行される
```fsharp
ctx.iter.range (0, 9) <| fun i ->
    ctx.print.s "aaa"
ctx.print.s "bbb"
```
以下の二つのコードは同じ動作になる。
```fsharp
ctx.iter.range (0, n-1) <| fun i ->
    print.t i
```
```fsharp
ctx.iter.num n <| fun i ->
    print.t i
```

以下のコードでは配列`a`の全要素に1を代入する
```fsharp
ctx.iter.range (0, a.size1-1) <| fun n ->
    a[n] <== 1
```
```fsharp
ctx.iter.num a.size1 <| fun n ->
    a[n] <== 1
```
配列の全要素にアクセスする場合など、反復処理の範囲が0から`a.size1-1`のような場合は以下のように記述できる。
```fsharp
a.foreach <| fun n ->
    a[n] <== n
```
反復処理の中に反復処理を記述することも可能。2次元配列の処理などで利用される。
```fsharp
ctx.iter.num 5 <| fun i ->
    ctx.iter.num 10 <| fun j ->
        ctx.print.tt i++j
```
2次元配列の全要素にアクセスする場合は以下のように記述できる。
```fsharp
a.foreach <| fun (i,j) ->
    a[i,j] <== i*j
```
`iter.loop`は無限ループ。`ex`はループを脱出する関数で、if式と組み合わせて使用される。`i`はループカウンタ
```fsharp
ctx.iter.loop <| fun (ex,i) ->
    ctx.print.s "aaa"
    x <== x - i*i
    ctx.br.if1 (i.>100) <| fun () ->
        ex() //ここでループ脱出
```
条件を満たす限り反復を繰り返す。
```fsharp
ctx.iter.whiledo (条件) <| fun ex ->
    (コード)
```

リストの各要素に対し処理を行う場合は`iter.list`を使用する。
```fsharp
ctx.iter.list [x;y;z] <| fun v ->
    ctx.print.t v
```
これは以下と同じ動作になる。
```fsharp
ctx.print.t x
ctx.print.t y
ctx.print.t z
```
