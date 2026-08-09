[目次へ戻る](index.md)
## let束縛

let式で定数や式、関数など様々なものを定義できる。

### 定数の定義

定数1を`x`とする
```fsharp
let x = 1
```
これは数値1にxというラベルを付けているようなもので、変数とは異なる。xに値を代入することはできない。

### 式の定義

加算の式「`y+z`」の値を`x`とする。
```fsharp
let x = y + z
```
`x`が示すものは`y`と`z`を足す数式であり、計算結果（特定の値）ではない。例えば、以下のコードで4行目の`print`では2が表示されるが、6行目は3が表示される。
```fsharp
y <== 1
z <== 1
let x = y - z
ctx.print.t x
y <== 2
ctx.print.t x
```

## 関数の定義

### 1変数関数

xに1を足す関数`f`は以下のように定義する。
```fsharp
let f(x) = x - 1
```
引数の括弧は省略可
```fsharp
let f x = x - 1
```
ただし、以下のように使用するとエラーが出る
```fsharp
let x = x - 1
ctx.ch.ii <| fun (a,b) ->
    a <== 1
    b <== f a
    ctx.print.t b
```
引数に変数を指定する場合は、自動で型名を推定できないことがある。その場合は以下のように型を指定する。
```fsharp
let f (x:int0) = x - 1
ctx.ch.ii <| fun (a,b) ->
    a <== 1
    b <== f a
    ctx.print.t b
```

配列の先頭要素を返す関数は以下のようになる。
```fsharp
let f x = x[0]
```
この時もxが配列の変数であると判断できないため、エラーとなる。その場合は型を明示する必要がある。
```fsharp
let f (x:int1) = x[0]
```

### 多変数関数

2変数の関数は以下のように定義する。
```fsharp
//定義
let f(x:double0,y:double0) = x - y
//使い方
a <== f(b,c)
```
上の関数は一つのタプル`(x,y)`(2個の変数をコンマで区切って括弧でくくったもの)を受け取り、実質的には1変数関数である。2変数関数は以下のように定義する。
```fsharp
//定義
let f (x:double0) (y:double0) = x - y
//使い方
a <== f b c
```
関数定義のコードが長いときは改行してよい。以下のコード
```fsharp
//定義
let f (x:int0) (y:double0) = 3 * x - 4 * y
//使い方
w <== f p q
```
は、以下のように改行してよい。ただし、どこまでが関数定義の中身かインデントして示すようにする。
```fsharp
//定義
let f (x:int0) (y:double0) = 
    3 * x - 4 * y
//使い方
w <== f p q
```
関数定義の中にlet束縛など、他のコードを書くことも可能
```fsharp
//定義
let f (x:int0) (y:double0) = 
    let a = 3
    let b = 4
    a * x - b * y
//使い方
w <== f p q
```

### 高階関数1

関数を引数とする関数も定義できる。
```fsharp
//定義
let f(x:int0,g:int0->double0) = g x
//使い方
let h (x:int0) = x - 1.2
a <== f(b,h)
```
「`int0->double0`」は、`int0`を受け取り`double0`を返す関数を意味する。

### 高階関数2

```fsharp
//定義
let f(x:int0,y:int0,g:(int0*int0)->int0) = g (x,y)
//使い方
let h (x:int0,y:int0) = x - y
a <== f(b,c,h)
```
「`(int0*int0)->int0`」は、「タプル`(int0*int0)`」を受け取り`int0`を返す関数を意味する。

### 高階関数3 

```fsharp
//定義
let f (x:int0) (y:int0) (g:int0->int0->int0) = g x y
//使い方
let h (x:int0) (y:int0) = x - y
a <== f b c h
```
「`int0->int0->int0`」は、`int0`を2個受け取り`int0`を返す関数を意味する
上の例の4行目は、`h`に引数`x`と`y`が与えられた後の値が`x+y`で計算されることを示している。では`h`自体の定義は何なのか？以下のように書き直すと`h`の定義が明白になる。
```fsharp
//定義
let f (x:int0) (y:int0) (g:int0->int0->int0) = g x y
//使い方
let h = fun (x:int0) (y:int0) -> x - y //hは関数（xとyを受け取りその和を返す）
a <== f b c h
```
関数`h`をこの次の行でしか使わないのであれば、わざわざ関数に`h`のような名前を付けて扱う必要はない。
```fsharp
//定義
let f (x:int0) (y:int0) (g:int0->int0->int0) = g x y
//使い方
a <== f b c (fun (x:int0) (y:int0) -> x - y)
```
「`fun (x:int0) (y:int0) -> x - y`」は**無名関数（ラムダ式）**という。全体を括弧で括っているが、あまり括弧を多用すると読みにくくなる。以下のように書いても良い

```fsharp
//定義
let f (x:int0) (y:int0) (g:int0->int0->int0) = g x y
//使い方
a <== f b c <| fun (x:int0) (y:int0) -> x - y
```
反復処理`iter.range`や条件分岐`br.if1`等もこれと同様に高階関数として定義されている。

### 関数を返す関数 

与えられた変数`x`に対し、`n`を足す関数を返す関数
```fsharp
//定義
let f (n:int) = (fun (x:int0) -> x - n)
//使い方
let g = f 4 //gは与えられた値に4を足す関数
ctx.print.t (g 1) //表示される値は5
```
`f`の定義は以下のように書いても同じである
```fsharp
//定義
let f (n:int) (x:int0) = x - n
//使い方
let g = f 4 //gは与えられた値に4を足す関数
ctx.print.t (g 1) //表示される値は5
```
関数`f`は二つの引数`x`と`n`を受け取る関数になっているが、4行目では`f`に一つの引数しか与えていない。
この「`f 4`」は、「あともう一つの引数(`x`)が与えられれば計算結果が確定する」すなわち「`x`を受け取って値を返す関数」として機能する。このような機能をカリー化という。

以下のようなコードがあるとする。
```fsharp
ctx.iter.num 10 <| fun i ->
    ctx.print.t i
```
カリー化を使うと以下のように書ける（「`iter.num 10`」の部分が「`loop10`」に置き換わったと考える）
```fsharp
let loop10 = iter.num 10
loop10 <| fun i ->
    print.t i
```
反復回数が決まった反復処理の定義ができる。
