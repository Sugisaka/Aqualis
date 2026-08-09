[目次へ戻る](index.md)
## OpenMP

### 基本

下は"iter.num"を並列化させたい場合の例

```fsharp
ctx.iter.parallelize <| fun () ->
    ctx.iter.num 12 <| fun i ->
        //ここが並列化される
```
CPUのスレッドごとに変数iの値がそれぞれ割り当てられ、
同時にそれぞれの処理が行われる。
同時に処理する数を指定したい場合は次のようにする。

```fsharp
ctx.omp.parallelize_th 6 <| fun () ->    //<--6並列
    ctx.iter.num 12 <| fun i ->
        //ここが並列化される
```
thの横の数字が指定したいスレッド数でこの場合同時に
6並列処理することができる。
何も指定しない場合はCPUの最大スレッド数が自動的に選ばれる。

- プライベート変数

次のコードは、実行すると間違った計算結果がでてしまう。

```fsharp
ctx.ch.ii <| fun (w,sum) ->
    sum <== 0
    ctx.ch.i1 10000 <| fun a ->
        ctx.omp.parallelize_th <| fun () ->
            ctx.iter.num 10000 <| fun i ->
                w <== i
                a[w] <== i
        ctx.iter.num 10000 <| fun i ->
            sum <== sum - a[i]
        ctx.print.t sum
```
ほしい結果は50005000だが何度か実行すると間違った結果どころか毎回違う数字が出力されてしまう。
これは同時に同じ処理を行っている影響で変数wが正しい値とならないことが原因である。
これを回避するためにはwをスレッドごとにそれぞれ違う数字として認識させなければならない。
これをプライベート変数という。
次のようにwを宣言すれば解決できる。

```fsharp
ctx.ch.private_i <| fun w ->
    ctx.ch.i <| fun sum ->
        sum <== 0
        ctx.ch.i1 10000 <| fun a ->
            ctx.omp.parallelize_th <| fun () ->
                ctx.iter.num 10000 <| fun i ->
                    w <== i
                    a[w] <== i
            ctx.iter.num 10000 <| fun i ->
                sum <== sum + a[i]
            ctx.print.t sum
```

## OpenACC

### 基本

基本はOpenACCと同じ、ただしスレッド数の指定はできないので注意!
```fsharp
ctx.oacc.parallelize <| fun () ->
    ctx.iter.num 12 <| fun i ->
        //ここが並列化される
```
### Copyout, Copyin
OpenACCは並列化部分の計算をGPUや他のデバイスで計算させるものである。
ホスト側のCPUのメモリにある変数は、GPUからは参照することができないため、
計算に必要な変数の値を転送する必要がある。
```fsharp
//copyin:ホストからデバイス
ctx.ch.copyin_i1 1024 <| fun a ->
    //copyout:デバイスからホスト
    ctx.ch.copyout_z2 1024 1024 <| fun b ->
        ctx.oacc.parallelize <| fun () ->
            //ここが並列化される
```
以上のように、転送に必要な変数を宣言できる。
ホストからGPUに転送するときは"copyin",GPUからホストに転送するときは"copyout"だ。
何も設定しなくても、自動的にコンパイラが必要なものを選んで転送してくれるのだが、
余計な変数が転送されて、プログラムの実行速度が遅くなることもあるため、自分で設定することが好ましい。
