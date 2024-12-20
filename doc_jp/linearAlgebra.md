[目次へ戻る](index.md)
## 線形代数演算

`open Aqualis`の下の行に

```fsharp
open Aqualis.lapack
```

を追加しておく。

### 単独の連立方程式

連立方程式

$$
\begin{align}
A\boldsymbol{x} = \boldsymbol{b}
\tag{1}
\end{align}
$$

を解く。 $A$ は2次元配列、 $\boldsymbol{b}$ は1次元配列。例えば、

$$
\begin{align}
A=
\begin{bmatrix}
1 & 2 \\
3 & 4 
\end{bmatrix}
,
\boldsymbol{b}=
\begin{bmatrix}
5 \\
6 
\end{bmatrix}
\tag{2}
\end{align}
$$

のとき、`A`と`b`を

```fsharp
A[1,1] <== 1.0
A[1,2] <== 2.0
A[2,1] <== 3.0
A[2,2] <== 4.0
b[1] <== 5.0
b[2] <== 6.0
```

とする。

```fsharp
La.solve_simuleq(A,b)
```

とすると、`b`に連立方程式の解 $A^{-1}\boldsymbol{b}$ が代入された状態になる。

サンプル：式(1)、(2)の求解（ $x_1=-4$ 、 $x_2=-4.5$ ）

```fsharp
//#############################################################################
// Test: simultaneous equation
let projectname = "test_simuleq"
let version = "1.0.0"
//#############################################################################
 
let outputdir = @"C:\home\work"

#I "C:\\Aqualis\\lib\\184_0_1_0"
#r "Aqualis.dll"
#load "version.fsx"

let fullversion = preprocess.backup outputdir __SOURCE_DIRECTORY__ __SOURCE_FILE__ projectname version
 
open Aqualis
 
Compile [Fortran] outputdir projectname fullversion <| fun () ->
    ch.d2 2 2 <| fun A ->
        ch.d1 2 <| fun b ->
            A[1,1] <== 1.0
            A[1,2] <== 2.0
            A[2,1] <== 3.0
            A[2,2] <== 4.0
            b[1] <== 5.0
            b[2] <== 6.0
            La.solve_simuleq(A,b)
            b.foreach <| fun i -> print.cc i b[i]
```

### 複数の連立方程式の解

係数行列が同じ複数の連立方程式

$$
\begin{align}
A\boldsymbol{x}_1 &= \boldsymbol{b}_1 \\
A\boldsymbol{x}_2 &= \boldsymbol{b}_2 \\
&\vdots \\
A\boldsymbol{x}_N &= \boldsymbol{b}_N
\tag{3}
\end{align}
$$

を解く。 $\boldsymbol{b}_1, \boldsymbol{b}_2, \cdots, \boldsymbol{b}_N$ を並べた2次元配列`b`を用意し

```fsharp
La.solve_simuleqs(A,b)
```

とすると、`b`に連立方程式の解 $A^{-1}\boldsymbol{x}_1, A^{-1}\boldsymbol{x}_2, \cdots, A^{-1}\boldsymbol{x}_N$ が代入された状態になる。
