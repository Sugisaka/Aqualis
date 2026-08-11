# Aqualis

- [インストール](#インストール) 
- [ソースファイルの実行](#ソースファイルの実行)
- [プリアンブル部](#プリアンブル部)
- [コメント文](#コメント文)
- [変数の定義と代入](#変数の定義と代入)
- [画面出力](#画面出力)
- [四則演算](#四則演算)
- [Aqualis数学関数](#Aqualis数学関数)
- [配列](#配列)
- [条件分岐](#条件分岐)
- [反復処理](#反復処理)
- [式と関数](#式と関数)
- [ファイル入出力](#ファイル入出力)
- [線形代数演算](#線形代数演算)
- [OpenMP](#OpenMP)
- [OpenACC](#OpenACC)
- [クラス定義例](#クラス定義例)
- [シンボリック微分](#シンボリック微分)

## インストール
[トップへ戻る](#aqualis)
1. .NET 10 SDKをインストールする。Visual StudioまたはBuild Tools for Visual Studioを使用する場合は、「F#デスクトップ言語のサポート」も選択する。
2. リポジトリのルートで次を実行し、Release版の`Aqualis.dll`を生成する。
    ```powershell
    dotnet build Aqualis.fsproj -c Release
    ```
3. 次を実行すると、ライブラリのバージョンに対応する`C:\Aqualis\lib\(バージョン番号)`へDLLがコピーされる。
    ```powershell
    dotnet fsi install.fsx
    ```
   DLLを別の場所へ手動でコピーした場合は、F#スクリプトの`#I`もその場所に合わせて変更する。

## ソースファイルの実行
[トップへ戻る](#aqualis)
F#スクリプトファイル（拡張子：fsx）を編集し実行すると、以下のファイルが生成される。
- f90ファイル または cファイル
  - プログラムのソースファイル
- shファイル
  - ソースファイルのコンパイル・実行を自動処理するスクリプトファイル

## プリアンブル部 
[トップへ戻る](#aqualis)

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
  - HTMLSequenceDiagram
  - JavaScript
  - PHP
  - Numeric（ソースファイルを生成せず直接計算）
- 14行目：`ctx`はコード生成に使用するコンテキスト。他の名称に変更してもよい。

以下のコードでは、「`ctx.print.s "aaa"`」と「`ctx.print.s "bbb"`」がFortranのコードに変換される。「`ctx.print.s "ccc"`」はインデントが戻っているので出力の対象外となる。
```fsharp
Compile [Fortran] outputdir projectname version <| fun ctx ->
    ctx.print.s "aaa"
    ctx.print.s "bbb"
ctx.print.s "ccc"
```

## コメント文
[トップへ戻る](#aqualis)

- コメント
  - テキストを「`(*`」、「`*)`」で囲む。複数行に渡っても良い。
- 行コメント
  - 「`//`」より右側の文字から改行するまでコメント文となる

### 出力ソースファイル上のコメント

`ctx.group.comment`を使用して記述したコメント文は生成されるソースファイルにも反映される。
```fsharp
ctx.group.comment "このコメントは生成コードにも出力される"
```

### ドキュメントコメント

F#と同じドキュメントコメントを使用可能（生成されるソースコードには反映されない）

### コマンドのコメント化

コード生成の一部を条件に応じて無効化する場合は、`ctx.group.whenEnabled`を使用する。次の例では内部の処理は生成されない。
```fsharp
ctx.group.whenEnabled false <| fun () ->
    ctx.print.s "この行は生成されない"
```


以降では、Aqualisのコンテキスト名（`Compile`関数で指定）を`ctx`として説明する。

## 変数の定義と代入
[トップへ戻る](#aqualis)
変数の宣言と値の代入の仕方は以下のように書く。

```fsharp
ctx.ch.i <| fun x ->
```
`i`の部分は変数の型、`x`は変数名を表わす。
次の行からはインデント(字下げ)を行い、インデントが戻るまでの間、`x`が使用可能となる。
変数の型には以下のようなものがある。

生成先のコードで変数名を指定したいときは`ctx.ch.I`、`ctx.ch.D`、`ctx.ch.Z`を使用する。以下の例では、生成先のソースコードで変数`x`が`aaa`という名前で生成される。すでに同名の変数があるなど、変数を生成できないときの変数名は自動で設定される。
```fsharp
ctx.ch.I "aaa" <| fun x ->
```

|指定子|Aqualisの型名|変数の型|
|-----|-----|-----|
|i|int0|整数|
|d|double0|倍精度浮動小数点型|
|z|complex0|複素数型（倍精度）|
|I|int0|整数|
|D|double0|倍精度浮動小数点型|
|Z|complex0|複素数型（倍精度）|

変数への代入は、`<==`を使用する
```fsharp
ctx.ch.i <| fun x ->
    x <== 1
```

let束縛を用いると定数を定義できる。以下の`a`はint型、`b`はdouble型。
定数には代入できない。
```
let a = 1
let b = 1.234
```

整数型の変数に小数・複素数を代入しようとするとエラーとなる。倍精度小数点型の変数に整数を代入するのは問題ない。
同様に、倍精度浮動小数点型の変数に複素数を代入することはできない。

変数の使用可能範囲は`ctx.ch`の次の行からインデントが戻るまでとなる。以下のコードでは、「`x<==3`」の行でインデントが戻っているため、変数`x`を使用できない。

エラーあり：
```fsharp
ctx.ch.i <| fun x ->
    x <== 1
    x <== 2
x <== 3 //ここでxは使用できない
```
修正後：
```fsharp
ctx.ch.i <| fun x ->
    x <== 1
    x <== 2
    x <== 3
```

変数を複数定義する場合は以下のようにする。
```fsharp
ctx.ch.i <| fun x ->
    ctx.ch.i <| fun y ->
        x <== 1
        y <== 2
```
3～4行目は「`ctx.ch.i <| fun x ->`」と「`ctx.ch.i <| fun y ->`」の中にあるため、`x`と`y`が両方使用できる。
以下の例では、2行目から3行目にかけてインデントが戻っており、3行目以降は変数`x`を使用できない。
```fsharp
ctx.ch.i <| fun x ->
    x <== 1
ctx.ch.i <| fun y ->
    x <== 1　//ここでxは使用できない
    y <== 2
```
以下は同じ変数が複数定義されているが、2、5行目は1行目で定義された`x`、4行目は3行目で定義された`x`を参照することになる
```fsharp
ctx.ch.i <| fun x ->
    x <== 1
    ctx.ch.i <| fun x ->
        x <== 2
    ctx.print.t x
```
複数の変数を宣言していくと、インデントが急速に深くなって読みにくくなる。そのため、複数の変数を同時に定義できる関数が用意されている。
```fsharp
ctx.ch.ii <| fun (x,y) ->
    x <== 1
    y <== 2
```
「`ii`」は、整数型2個を表わす。最大4個まで指定可能
```fsharp
ctx.ch.iiii <| fun (x,y,z,w) ->
    x <== 1
    y <== 2
    z <== 3
    w <== 4
```
異なる型が混在していても良い。以下の例では、`x`が整数型、`y`と`z`が浮動小数点型、`w`が複素数型になる。
```fsharp
ctx.ch.iddz <| fun (x,y,z,w) ->
    x <== 1
    y <== 2.0
    z <== 3.0
    w <== 4.0+asm.uj*5.0
```
指定子の順番はi(整数)→d(実数)→z(複素数)の順に限られる。（「`ctx.ch.iiz`」は可能だが「`ctx.ch.izi`」「`ctx.ch.zid`」は不可）

### 複素数

例えば、変数`x`に 5 - 3jを代入する式は以下のようになる。

```fsharp
w <== 5.0-asm.uj*3.0
```
「`asm.uj`」は虚数単位を表わす。複素数型の変数については、''変数名.プロパティ''でいくつかの値が参照できる

|プロパティ|内容|
|--|--|
|abs|絶対値|
|re|実部|
|im|虚部|
|pha|偏角|
|pow|絶対値の2乗|

例えば、変数`a`に`x`の絶対値の2乗を代入する式は以下のようになる。
```fsharp
a <== x.pow
```

## 画面出力
[トップへ戻る](#aqualis)

`print`を用いて画面に文字や変数の値を出力できる。単純な文字列を表示する場合は以下のように書く。
```fsharp
ctx.print.s "Hello World!"
```
変数の値を表示する場合は以下のように書く。
```fsharp
ctx.print.t x
```
2個以上の変数、または文字列と変数の組み合わせを出力する場合は`++`演算子を用いて以下のように書く。
```
ctx.print.tt <| "aaa"++a++"bbb"++b
```

## 四則演算 
[トップへ戻る](#aqualis)

加算
```fsharp
z <== x + y
```
減算
```fsharp
z <== x - y
```
乗算
```fsharp
z <== x * y
```
除算（`x`と`y`が`int`、`double`のいずれであっても、`z`は浮動小数点型になる）
```fsharp
z <== x / y
```
除算(`x`と`y`は整数。`z`は小数点以下を切り捨てて整数型となる)
```fsharp
z <== x ./ y
```
剰余
```fsharp
z <== x % y
```
符号反転
```fsharp
z <== -x
```

## Aqualis数学関数

以下の数学関数が定義されている

|表記例|意味|
|--|--|
|`asm.uj`|虚数単位|
|`asm.pi`|円周率|
|`asm.abs(x)`|変数xの絶対値|
|`asm.pow(x,y)`|変数xのy乗|
|`asm.exp(x)`|指数関数|
|`asm.conj(x)`|変数xの共役複素数|
|`asm.sin(x)`|正弦関数|
|`asm.cos(x)`|余弦関数|
|`asm.tan(x)`|正接関数|
|`asm.asin(x)`|逆正弦関数|
|`asm.acos(x)`|逆余弦関数|
|`asm.atan(x)`|逆正接関数($-\pi/2$～$\pi/2$の範囲で出力)|
|`asm.atan2(y,x)`|$y/x$の逆正接関数($-\pi$～$\pi$の範囲で出力)|
|`asm.log(x)`|自然対数|
|`asm.log10(x)`|常用対数|
|`asm.sqrt(x)`|平方根|
|`asm.floor(x)`|変数xの小数点以下切り捨て|
|`asm.ceil(x)`|変数xの小数点以下切り上げ|
|`asm.toint(x)`|浮動小数点型を整数型に変換|
|`asm.todouble(x)`|整数型を浮動小数点型に変換|

### ハンケル関数

#### 第2種0次ハンケル関数

$H^{(2)}_0(x)$を計算。関数の値は`h`に保存されている

```fsharp
asm.besselh0 x <| fun h ->
    ctx.print.t h
```

#### 第2種1次ハンケル関数

$H^{(2)}_1(x)$を計算。関数の値は`h`に保存されている

```fsharp
asm.besselh1 x <| fun h ->
    ctx.print.t h
```

## 配列
[トップへ戻る](#aqualis)

1～3次元配列を指定可能。Aqualis上での型名は、例えば整数型1次元配列の場合`int1`、複素数型3次元配列の場合`complex3`となる。

### 1次元配列

1次元配列は次のコードで配列を生成することができる。`i`は変数の型(他に`d`、`z`が指定可)、1は配列の次元、5が要素数、aが変数名になっている。上のコードでは「1次元で要素数が5の配列」を生成することができる。
```fsharp
ctx.ch.i1 5 <| fun a ->
```
以下のように、要素数が未定の配列を宣言し、後から要素数を指定しても良い。
```fsharp
ctx.ch.i01 <| fun a ->
    a.allocate(5)
    a.deallocate()
```
`allocate`関数の引数は要素数を表わし、必要なメモリを確保する関数。
`deallocate`は確保したメモリを解放する関数。
`deallocate`の後は要素数が0となり、再び`allocate`関数でメモリを確保するまで配列を使用できなくなる。

- 以下は配列の変数名を「a」とした場合の例
  - 要素数は整数値、int型、int0型のいずれかで指定する
  - a.clear()で要素の値をすべて0に初期化する（配列以外の変数にも使用可能）
  - 配列の要素は角括弧で指定する。Aqualis上の要素インデックスは0から始まり、要素数が`a.size1`なら有効範囲は`0`～`a.size1-1`となる。Fortranの生成コードでは、Aqualisが添字を1始まりに変換する。範囲外アクセスを避けるため、必要に応じてデバッグモードの境界チェックも利用する
  - `a.size1`で配列の要素数を参照できる

```fsharp
ctx.ch.i1 5 <| fun a ->
    //配列aの先頭要素に5を代入
    a[0] <== 5
    //配列aの第2要素に10を代入
    a[1] <== 10
    //配列aの最終要素に10を代入
    a[a.size1-1] <== 10
    //以下はエラー
    a[5] <== 10
    a[-1] <== 10
```

### 2次元配列

2次元配列は、同じ型の変数が縦横に2次元的に並んでいるイメージ。画像や平面上の電界分布等を表わすのによく使われる
次のコードで2次元配列を生成できる。
```fsharp
ctx.ch.i2 (3,5) <| fun a ->
```
- 全要素を0で初期化するときは`a.clear()`と記述する
- 配列の要素にアクセスするときは「`a[1,2]`」のように入力する。
- 配列aの第1インデックスの要素数は`a.size1`、第2インデックスの要素数は`a.size2`で参照できる。

### 3次元配列

次のコードで3次元配列を生成できる。
```fsharp
ctx.ch.i3 (3,4,5) <| fun a ->
```

### 部分配列

3個の配列`x`,`y`,`z`に対して、以下の要素同士の四則演算、代入式
```fsharp
ctx.iter.num z.size1 <| fun i ->
    z[i] <== x[i] - y[i]
```
は次のように書ける（2、3次元配列も同様）
```fsharp
z <== x - y
```
全要素でなく、要素の範囲を指定することもできる。1次元配列の第1～3要素のみ演算する場合は、
```fsharp
z[(1,3)] <== x[(1,3)] - y[(1,3)]
```
とする。`()`は先頭から末尾までのすべての要素を表す。例えば、2次元配列の第4列の全要素を演算する場合は
```fsharp
z[(),4] <== x[(),4] - y[(),4]
```
とする。

## 条件分岐
[トップへ戻る](#aqualis)

基本的に3種類の書き方がある

### パターン1
"条件1"を満たすとき、"コード1"を実行
```fsharp
ctx.br.if1 (条件1) <| fun () ->
    (コード1)
```

### パターン2
"条件1"を満たすとき、"コード1"を実行。"条件1"を満たさないときは、"コード2"を実行
```fsharp
ctx.br.if2 (条件1)
<| fun () ->
    (コード1)
<| fun () ->
    (コード2)
```

### パターン3
"条件1"を満たすとき、"コード1"を実行。"条件1"を満たさず"条件2"を満たすときは、"コード2"を実行。"条件1"、"条件2"を満たさず"条件3"を満たすとき、"コード3"を実行。
```fsharp
ctx.br.branch <| fun b ->
    b.IF (条件1) <| fun () ->
        (コード1)
    b.IF (条件2) <| fun () ->
        (コード2)
    b.IF (条件3) <| fun () ->
        (コード3)
```
"条件1"を満たすとき、"コード1"を実行。"条件1"を満たさず"条件2"を満たすときは、"コード2"を実行。"条件1"、"条件2"を満たさず"条件3"を満たすとき、"コード3"を実行。"条件1"、"条件2"、"条件3"のいずれも満たさないとき、コード4を実行
```fsharp
ctx.br.branch <| fun b ->
    b.IF (条件1) <| fun () ->
        (コード1)
    b.IF (条件2) <| fun () ->
        (コード2)
    b.IF (条件3) <| fun () ->
        (コード3)
    b.EL <| fun () ->
        (コード4)
```
この`b.IF`はいくつでも記述して分岐することができる。条件分岐の中に別の分岐を入れる場合は以下のようにする。
```fsharp
ctx.br.branch <| fun b1 ->
    b1.IF (条件1A) <| fun () ->
        (コード1A)
    b1.IF (条件2) <| fun () ->
        ctx.br.branch <| fun b2 ->
            b2.IF (条件2A) <| fun () ->
                (コード2A)
            b2.IF (条件2B) <| fun () ->
                (コード2B)
    b1.IF (条件1B) <| fun () ->
        (コード1B)
    b1.EL <| fun () ->
        (コード1C)
```

### 条件式

比較演算子

|条件式|説明|
|--|--|
|`x.>y`|`x`が`y`より大きいとき真|
|`x.<y`|`x`が`y`より小さいとき真|
|`x.>=y`|`x`が`y`以上のとき真|
|`x.<=y`|`x`が`y`以下のとき真|
|`x.=y`|`x`と`y`が等しいとき真|
|`x.=/y`|`x`と`y`が等しくないとき真|

複数の条件式を組み合わせてもよい。`x.<y`と`y.<z`を同時に満たすとき真となる式は
```fsharp
And [x.<y; y.<z]
```
または以下のようにも書ける。
```fsharp
x.<y.<z
```

`x.<y`と`y.<z`の少なくともいずれかを満たすとき真
```fsharp
Or [x.<y; y.<z]
```

## 反復処理 
[トップへ戻る](#aqualis)

以下のコードで反復処理を指定する

`i`は整数型の変数で、`i`が1から10まで1ずつ増加しながら処理が繰り返し実行される。反復処理の対象はこのコードの次の行以降。反復処理の範囲はインデントで表わす。
```fsharp
ctx.iter.range (1, 10) <| fun i ->
```

以下の例では`aaa`が画面に10回表示される。「`ctx.print.s "bbb"`」の行はインデントが戻っているため10回の反復処理の後に1度だけ実行される
```fsharp
ctx.iter.range (0, 9) <| fun i ->
    ctx.print.s "aaa"
ctx.print.s "bbb"
```
以下の二つのコードは同じ動作になる。
```fsharp
ctx.iter.range (0, n-1) <| fun i ->
    ctx.print.t i
```
```fsharp
ctx.iter.num n <| fun i ->
    ctx.print.t i
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
        ctx.print.tt <| i++j
```
2次元配列の全要素にアクセスする場合は以下のように記述できる。
```fsharp
a.foreach <| fun (i,j) ->
    a[i,j] <== i*j
```
`ctx.iter.loop`は無限ループ。`ex`はループを脱出する関数で、if式と組み合わせて使用される。`i`はループカウンタ
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

リストの各要素に対し処理を行う場合は`ctx.iter.list`を使用する。
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

## 式と関数
[トップへ戻る](#aqualis)

### let束縛

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
let x = y + z
ctx.print.t x
y <== 2
ctx.print.t x
```

### 1変数関数

xに1を足す関数`f`は以下のように定義する。
```fsharp
let f(x) = x + 1
```
引数の括弧は省略可
```fsharp
let f x = x + 1
```
ただし、以下のように使用するとエラーが出る
```fsharp
let f x = x + 1
ctx.ch.ii <| fun (a,b) ->
    a <== 1
    b <== f a
    ctx.print.t b
```
引数に変数を指定する場合は、自動で型名を推定できないことがある。その場合は以下のように型を指定する。
```fsharp
let f (x:int0) = x + 1
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

```
以下の二つの関数は、引数の型が異なるだけで同じ処理を行っている。
```fsharp
let f(a:double0,b:int0) =
    a <== b + 1
let f(a:double0,b:double0) =
    a <== b + 1
```
以下のように一つの関数だけ定義しておくと、引数`b`に`int0`と`double0`の両方を指定できる。
```fsharp
let inline f(a:double0, b:#IReal0) =
    a <== b.ToDouble0 + 1
```
`IReal0`は実数を表すインターフェースである。したがって、引数`b`に`complex0`型の変数を指定するとエラーになる。
```
以下の三つの関数は、引数の型が異なるだけで同じ処理を行っている。
```fsharp
let f(a:complex0,b:int0) =
    a <== b + 1
let f(a:complex0,b:double0) =
    a <== b + 1
let f(a:complex0,b:complex0) =
    a <== b + 1
```
以下のように一つの関数だけ定義しておくと、引数`b`に`int0`、`double0`、`double0`のいずれも指定できる。
```fsharp
let inline f(a:double0, b:#INum0) =
    a <== b.ToComplex0 + 1
```
`INum0`は数値を表すインターフェースである。

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
上の例では、`h`に引数`x`と`y`が与えられた後の値が`x-y`で計算される。では`h`自体の定義は何なのか？以下のように書き直すと`h`の定義が明白になる。
```fsharp
//定義
let f (x:int0) (y:int0) (g:int0->int0->int0) = g x y
//使い方
let h = fun (x:int0) (y:int0) -> x - y //hは関数（xとyを受け取り、その差を返す）
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
反復処理`ctx.iter.range`や条件分岐`ctx.br.if1`等もこれと同様に高階関数として定義されている。

### 関数を返す関数 

与えられた変数`x`に対し、`n`を足す関数を返す関数
```fsharp
//定義
let f (n:int) = (fun (x:int0) -> x + n)
//使い方
let g = f 4 //gは与えられた値に4を足す関数
ctx.print.t (g 1) //表示される値は5
```
`f`の定義は以下のように書いても同じである
```fsharp
//定義
let f (n:int) (x:int0) = x + n
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
カリー化を使うと以下のように書ける（「`ctx.iter.num 10`」の部分が「`loop10`」に置き換わったと考える）
```fsharp
let loop10 = ctx.iter.num 10
loop10 <| fun i ->
    ctx.print.t i
```
反復回数が決まった反復処理の定義ができる。

## ファイル入出力
[トップへ戻る](#aqualis)

コード上で出力した値を別のファイルに保存したり、また逆にファイルに保存されたデータを使いたい場合がある。以下にその方法を記す。

### ファイルへの書き込み 

変数`x`、`y`、`z`の値をファイル「`test.dat`」に書き込む
```fsharp
ctx.ch.id <| fun (x,y) ->
    x <== 1
    y <== 2.0
    ctx.io.fileOutput "test.dat" <| fun wr ->
        wr.t "aaa" //文字列を書き込み
        wr.t x //変数を書き込み
        wr.tt <| x++y //2個以上の変数をタブ区切りで書き込み
```
以下はエラーになる
```fsharp
let x = 1
let y = 2.0
ctx.io.fileOutput "test.dat" <| fun wr ->
    wr.tt <| x++y
```
複数のファイルを同時に開くことも可能。その際は書き込み指定子「`wr`」の名前を変える
```fsharp
ctx.ch.id <| fun (x,y) ->
    x <== 1
    y <== 2.0
    ctx.io.fileOutput "test1.dat" <| fun wr1 ->
        ctx.io.fileOutput "test2.dat" <| fun wr2 ->
            wr1.t x //test1.datに書き込み
            wr2.t x //test2.datに書き込み
```
ファイル名は整数の変数を指定することも可能
```fsharp
ctx.ch.i <| fun n ->
    n <== 4
    ctx.io.fileOutput ("test"++n++".dat") <| fun wr -> //ファイル名は「test00004.dat」
```

### ファイルからの読み込み 

以下のような内容のテキストファイル"test.dat"があるとき、
```
           3    -1.230000000000000E+001
```
ファイルの読み込みは以下のようにする。
```fsharp
ctx.ch.id <| fun (x,y) ->
    ctx.io.fileInput "test.dat" <| fun rd ->
        rd <| x++y
```

## 線形代数演算
[トップへ戻る](#aqualis)

### 単独の連立方程式

連立方程式

$$
\begin{align}
A\boldsymbol{x} = \boldsymbol{b}
\end{align}
$$

を解く。 $A$ は2次元配列、 $\boldsymbol{b}$ は1次元配列。例えば、

$$
\begin{align}
A &=
\begin{bmatrix}
1 & 2 \\
3 & 4 
\end{bmatrix}
,
\\
\boldsymbol{b} &=
\begin{bmatrix}
5 \\
6 
\end{bmatrix}
\end{align}
$$

のとき、`A`と`b`を

```fsharp
A[0,0] <== 1.0
A[0,1] <== 2.0
A[1,0] <== 3.0
A[1,1] <== 4.0
b[0] <== 5.0
b[1] <== 6.0
```

とする。

```fsharp
ctx.la.solve_simuleq(A,b)
```

とすると、`b`に連立方程式の解 $A^{-1}\boldsymbol{b}$ が代入された状態になる。

サンプル：上記の連立方程式の求解（ $x_1=-4$ 、 $x_2=4.5$ ）

```fsharp
//#############################################################################
// Test: simultaneous equation
let projectname = "test_simuleq"
let version = "1.0.0"
//#############################################################################
 
let outputdir = @"C:\home\work"

#I "C:\\Aqualis\\lib\\188_0_0_0"
#r "Aqualis.dll"
 
open Aqualis
 
Compile [Fortran] outputdir projectname version <| fun ctx ->
    ctx.ch.d2 (2,2) <| fun A ->
        ctx.ch.d1 2 <| fun b ->
            A[0,0] <== 1.0
            A[0,1] <== 2.0
            A[1,0] <== 3.0
            A[1,1] <== 4.0
            b[0] <== 5.0
            b[1] <== 6.0
            ctx.la.solve_simuleq(A,b)
            b.foreach <| fun i -> ctx.print.tt <| i++b[i]
```

### 複数の連立方程式の解

係数行列が同じ複数の連立方程式

$$
\begin{align}
A\boldsymbol{x}_1 &= \boldsymbol{b}_1 \\
A\boldsymbol{x}_2 &= \boldsymbol{b}_2 \\
&\vdots \\
A\boldsymbol{x}_N &= \boldsymbol{b}_N
\end{align}
$$

を解く。 $\boldsymbol{b}_1, \boldsymbol{b}_2, \cdots, \boldsymbol{b}_N$ を並べた2次元配列`b`を用意し

```fsharp
ctx.la.solve_simuleqs(A,b)
```

とすると、`b`に連立方程式の解 $A^{-1}\boldsymbol{b}_1, A^{-1}\boldsymbol{b}_2, \cdots, A^{-1}\boldsymbol{b}_N$ が代入された状態になる。

## OpenMP
[トップへ戻る](#aqualis)

### 基本

以下は`ctx.iter.num`を並列化する例

```fsharp
ctx.omp.parallelize <| fun pctx ->
    pctx.iter.num 12 <| fun i ->
        //ここが並列化される
```
CPUのスレッドごとに変数iの値がそれぞれ割り当てられ、
同時にそれぞれの処理が行われる。
同時に処理する数を指定したい場合は次のようにする。

```fsharp
ctx.omp.parallelize_th 6 <| fun pctx ->    //<--6並列
    pctx.iter.num 12 <| fun i ->
        //ここが並列化される
```
thの横の数字が指定したいスレッド数でこの場合同時に
6並列処理することができる。
何も指定しない場合はCPUの最大スレッド数が自動的に選ばれる。

- プライベート変数

並列化した反復処理のループカウンタは、スレッドごとのプライベート変数として自動的に生成される。共有変数を作業用変数として更新するとデータ競合が起きるため、可能な限りループカウンタをそのまま使用する。

```fsharp
ctx.ch.i <| fun sum ->
    sum <== 0
    ctx.ch.i1 10000 <| fun a ->
        ctx.omp.parallelize <| fun pctx ->
            pctx.iter.num 10000 <| fun i ->
                a[i] <== i+1
        ctx.iter.num 10000 <| fun i ->
            sum <== sum + a[i]
        ctx.print.t sum
```
この例では、配列への書き込みだけを並列化し、共有変数`sum`の加算は並列領域の外で行う。加算自体を並列化する場合は、`ctx.omp.reduction`または`ctx.omp.reduction_th`を使用する。

## OpenACC

### 基本

基本的な書き方はOpenMPと同じ。ただしスレッド数は指定できず、生成対象言語はFortranまたはC99に限られる。
```fsharp
ctx.oacc.parallelize <| fun pctx ->
    pctx.iter.num 12 <| fun i ->
        //ここが並列化される
```

OpenACCは並列化部分をGPUなどのアクセラレータで実行する。現バージョンの公開APIには、旧版の`ctx.ch.copyin_*`、`ctx.ch.copyout_*`ヘルパーはないため使用しない。

## クラス定義例
[トップへ戻る](#aqualis)

以下のコードでクラス`testClass1`を定義できる。Fortran、C言語ではフィールドを構造体、メソッドをインライン展開して実装される。

```fsharp
/// <summary>
/// testClass1
/// </summary>
type testClass1(sname_,name,ctx:Aqualis) =
    inherit structureValue<testClass1>(sname_,name,ctx)
    /// クラス名
    static member sname = "testClass1"
    /// コンストラクタ
    new(name,ctx:Aqualis) =
        ctx.str.reg(testClass1.sname,name)
        testClass1(testClass1.sname,name,ctx)
    override _.Rewrap(n,targetEnvironment) = testClass1(sname_,n,targetEnvironment)
    /// フィールド1
    member public __.n1 = ctx.str.i0(sname_,name,"x1")
    /// フィールド2
    member public __.x1 = ctx.str.d0(sname_,name,"y1")
    /// フィールド3
    member public __.z1 = ctx.str.z0(sname_,name,"x2")
        
/// <summary>
/// testClass1の配列
/// </summary>
type testClass1_1(sname_,name,size1,ctx:Aqualis) =
    inherit structureArray1<testClass1,testClass1_1>(sname_,name,size1,ctx)
    new(name,size1,ctx:Aqualis) =
        ctx.str.reg(testClass1.sname,name,size1)
        testClass1_1(testClass1.sname,name,A1 size1,ctx)
    new(name,ctx:Aqualis) = testClass1_1(name,0,ctx)
    override _.WrapElement n = testClass1(sname_,n,ctx)
    override _.Rewrap(n,v,targetEnvironment) = testClass1_1(sname_,n,v,targetEnvironment)
    /// このクラスを別のクラスのフィールドにする場合は以下のメソッドも定義する
    static member str_mem(psname, vname, name, size1,ctx:Aqualis) =
        ctx.str.addmember(psname,(Structure(testClass1.sname),size1,name))
        testClass1_1(testClass1.sname,ctx.str.mem(vname,name), size1,ctx)
```

### 使用例

```fsharp
Compile [Fortran] outputdir projectname version <| fun ctx ->
    
    //testClass1型変数（変数名：u）を生成
    let u = testClass1("u",ctx)
    //フィールドへのアクセスは「変数名.フィールド名」
    u.n1 <== 1
    u.x1 <== 2.0
    u.z1 <== 3.0+asm.uj*4.0
    ctx.print.tt <| u.n1 ++ u.x1 ++ u.z1
    
    //testClass1型1次元配列（配列名：v）を生成
    let v = testClass1_1("v",ctx)
    //配列要素数を指定してメモリ確保
    v.allocate(10)
    //配列へのアクセス
    v.foreach <| fun i ->
        v[i].n1 <== 1
        v[i].x1 <== 2.0
        v[i].z1 <== 3.0+asm.uj*4.0
        ctx.print.tt <| v[i].n1 ++ v[i].x1 ++ v[i].z1
```

## シンボリック微分
[トップへ戻る](#aqualis)

関数 $f(x)$ とその微分 $g(x)=\mathrm{d}f(x)/\mathrm{d}x$ を代数的に計算する

$$
\begin{align}
f(x) &= 2x^2+3x
\\
g(x) &= \frac{\mathrm{d}f(x)}{\mathrm{d}x}
\end{align}
$$

```fsharp
ctx.ch.d <| fun x ->
    //関数f
    let f(x:double0) = 2*x*x+3*x
    //関数fのx微分
    let g(x:double0) = asm.diff(f x,x)
    
    //微分値の確認
    ctx.iter.num 100 <| fun i ->
        x <== 0.1*i
        ctx.print.tt <| x ++ (g x) ++ (4*x+3)
```

関数を含む式も微分できる。

$$
\begin{align}
f(x) &= 2x\sin(3x)
\\
g(x) &= \frac{\mathrm{d}f(x)}{\mathrm{d}x}
\end{align}
$$

```fsharp
ctx.ch.d <| fun x ->
    //関数f
    let f(x:double0) = 2*x*asm.sin(3*x)
    //関数fのx微分
    let g(x:double0) = asm.diff(f x,x)
    
    //微分値の確認
    ctx.iter.num 100 <| fun i ->
        x <== 0.1*i
        ctx.print.tt <| x ++ (g x) ++ (2*asm.sin(3*x)+6*x*asm.cos(3*x))
```

$$
\begin{align}
f(x) &= \frac{2\sin(3x)}{\sqrt{x^2+1}}
\\
g(x) &= \frac{\mathrm{d}f(x)}{\mathrm{d}x}
\end{align}
$$

```fsharp
ctx.ch.d <| fun x ->
    //関数f
    let f(x:double0) = 2*asm.sin(3*x)/asm.sqrt(x*x+1)
    //関数fのx微分
    let g(x:double0) = asm.diff(f x,x)
    
    //微分値の確認
    ctx.iter.num 100 <| fun i ->
        x <== 0.1*i
        ctx.print.tt <| x ++ (g x) ++ (6*asm.cos(3*x)/asm.sqrt(x*x+1) - 2*x*asm.sin(3*x)/asm.pow(x*x+1,1.5))
```

級数の微分

$$
\begin{align}
f(x) &= 2\sum_{i=1}^5 (ix^2+1)
\\
g(x) &= \frac{\mathrm{d}f(x)}{\mathrm{d}x}
\end{align}
$$

```fsharp
ctx.ch.d <| fun x ->
    //関数f
    let f(x:double0) = 2*(asm.dSum (1,5) <| fun i -> i*x*x+1)
    //関数fのx微分
    let g(x:double0) = asm.diff(f x,x)
    
    //微分値の確認
    ctx.iter.num 100 <| fun i ->
        x <== 0.1*i
        ctx.print.tt <| x ++ (g x) ++ (2*(asm.dSum (1,5) <| fun i -> 2*i*x))
```

級数を含む式の微分では、同じ級数の計算を何度も行うことがある（以下の例では2回）

$$
\begin{align}
f(x)
&= \frac{\displaystyle \left[2x+\sum_{i=1}^5 (ix^2+1)\right]^2}{x+1}
\\
g(x)
&= \frac{\mathrm{d}f(x)}{\mathrm{d}x}
\\
&= \frac{\displaystyle 2 \left[ 2x+\sum_{i=1}^5 (ix^2+1) \right] \left[ 2+\sum_{i=1}^5 (2ix) \right] }{x+1}
-\frac{\displaystyle \left[2x+\sum_{i=1}^5 (ix^2+1)\right]^2}{(x+1)^2}
\end{align}
$$

```fsharp
  ctx.ch.d <| fun x ->
      //関数f
      let f(x:double0) = asm.pow(2*x+(asm.dSum (1,5) <| fun i -> i*x*x+1),2)/(x+1)
      //関数fのx微分
      let g(x:double0) = asm.diff(f x,x)
      
      //微分値の確認
      ctx.iter.num 100 <| fun i ->
          x <== 0.1*i
          ctx.print.tt <| x ++ (g x) ++ (2*(2*x+(asm.dSum (1,5) <| fun i -> i*x*x+1))*(2+(asm.dSum (1,5) <| fun i -> 2*i*x))/(x+1)-asm.pow(2*x+(asm.dSum (1,5) <| fun i -> i*x*x+1),2)/asm.pow(x+1,2))
```

`asm.iLet`、`asm.dLet`、`asm.zLet`を使うと、一度計算した式の値を一時変数として再利用できる。整数式には`iLet`、実数式には`dLet`、複素数式には`zLet`を使用する。

```fsharp
  ctx.ch.d <| fun x ->
      //関数f
      let f(x:double0) =
          asm.dLet (asm.dSum (1,5) <| fun i -> i*x*x+1) <| fun tmp ->
              asm.pow(2*x+tmp,2)/(x+1)
      //関数fのx微分
      let g(x:double0) = asm.diff(f x,x)
      //微分値の確認
      ctx.iter.num 100 <| fun i ->
          x <== 0.1*i
          //数式内の級数を評価し、一時変数に保存
          (f x).eval()
          ctx.print.tt <| x ++ (g x)
```

配列要素による微分

```fsharp
    let N = 100
    ctx.ch.d1 N <| fun x ->
    ctx.ch.d1 N <| fun y ->
        //関数f
        let f(x:double1) = 2*(asm.dSum (1,N) <| fun i -> i*asm.pow(x[i],2)+1)
        //関数fのx[j]微分
        let g(x:double1,j:int0) = asm.diff(f x,x[j])
        //xの初期化
        ctx.iter.num N <| fun i ->
            x[i] <== 0.1*i
        //微分計算 y[i] = df/d(x[i])
        ctx.iter.num N <| fun j ->
            y[j] <== g (x,j)
        //微分値の確認
        ctx.iter.num N <| fun j ->
            ctx.print.tt <| j ++ y[j] ++ (2*j*2*x[j])
```
