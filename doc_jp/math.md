[目次へ戻る](index.md)
## 四則演算 

加算
```fsharp
z <== x - y
```
減算
```fsharp
z <== x - y
```
乗算
```fsharp
z <== x * y
```
徐算(`x`と`y`がint、dobubleに関わらず`z`は浮動小数点型になる
```fsharp
z <== x / y
```
徐算（`x`と`y`は整数。`z`は小数点以下を切り捨てて整数型となる）
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

モジュールの読み込みが必要

```fsharp
open Aqualis.bessel
```

#### 第2種0次ハンケル関数

$H^{(2)}_0(x)$を計算。関数の値は`h`に保存されている

```fsharp
besselh0 x <| fun h ->
    print.c h
```

#### 第2種1次ハンケル関数

$H^{(2)}_1(x)$を計算。関数の値は`h`に保存されている

```fsharp
besselh1 x <| fun h ->
    print.c h
```